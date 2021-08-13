use druid::{
    text::{AttributesAdder, RichText, RichTextBuilder, TextStorage},
    widget::prelude::*,
    Color, FontFamily, FontStyle, FontWeight, Point, Selector, TextLayout,
};
use pulldown_cmark::{CodeBlockKind, Event as ParseEvent, Parser, Tag};
use syntect::{easy::HighlightLines, highlighting::Style, util::LinesWithEndings};

const BLOCKQUOTE_COLOR: Color = Color::grey8(0x88);
const LINK_COLOR: Color = Color::rgb8(0, 0, 0xEE);
const OPEN_LINK: Selector<String> = Selector::new("druid-example.open-link");
const PARA_SPACING: f64 = 10.;

use crate::{SYNTAX, THEMES};

/// The markdown widget
pub struct Markdown {
    content: Vec<MdPart>,
    /// The position of each element.
    pos: Option<Vec<Point>>,
}

impl Markdown {
    pub fn new() -> Self {
        Markdown {
            content: vec![],
            pos: None,
        }
    }

    fn calculate_positions(&mut self) -> Size {
        let mut pos = Vec::with_capacity(self.content.len());
        let mut size = Size::new(0., 0.);
        let mut iter = self.content.iter().peekable();
        while let Some(el) = iter.next() {
            pos.push((0., size.height).into());
            size.width = size.width.max(el.size().width);
            size.height += el.size().height;
            if iter.peek().is_some() {
                size.height += PARA_SPACING;
            }
        }
        self.pos = Some(pos);
        size
    }
}

impl<T: TextStorage> Widget<T> for Markdown {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut T, env: &Env) {
        for el in &mut self.content {
            el.event(ctx, event, env);
        }
    }

    fn lifecycle(&mut self, _ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &T, _env: &Env) {
        match event {
            LifeCycle::WidgetAdded => {
                self.content = build_md_content(data);
                // self.pos is None already
            }
            _ => (),
        }
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &T, data: &T, env: &Env) {
        // rebuild the structure of the layout.
        if old_data.same(data) {
            for el in &mut self.content {
                el.needs_rebuild_after_update(ctx);
            }
            return;
        }

        self.content = build_md_content(data);
        self.pos = None;
    }

    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, _data: &T, env: &Env) -> Size {
        let width = bc.max().width;
        for el in &mut self.content {
            el.layout(ctx, bc, env);
        }
        let size = self.calculate_positions();
        bc.constrain(size)
    }

    fn paint(&mut self, ctx: &mut PaintCtx, _data: &T, env: &Env) {
        // We always calculate positions in layout.
        for (el, pos) in self.content.iter().zip(self.pos.as_ref().unwrap().iter()) {
            el.paint(ctx, env, *pos);
        }
    }
}

#[derive(Debug)]
pub enum MdPart {
    Paragraph {
        layout: TextLayout<RichText>,
    },
    Code {
        layout: TextLayout<RichText>,
    },
    Heading {
        level: u32,
        layout: TextLayout<RichText>,
    },
    Blockquote {
        content: Vec<MdPart>,
    },
}

impl MdPart {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, env: &Env) {
        match self {
            MdPart::Paragraph { layout } => link_event(ctx, event, layout),
            MdPart::Code { layout } => link_event(ctx, event, layout),
            MdPart::Heading { layout, .. } => link_event(ctx, event, layout),
            MdPart::Blockquote { content } => {
                for el in content {
                    el.event(ctx, event, env);
                }
            }
        }
    }

    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, env: &Env) {
        match self {
            MdPart::Paragraph { layout }
            | MdPart::Code { layout }
            | MdPart::Heading { layout, .. } => {
                layout.set_wrap_width(bc.max().width);
                layout.rebuild_if_needed(ctx.text(), env);
            }
            MdPart::Blockquote { content } => {
                for part in content {
                    part.layout(ctx, bc, env);
                }
            }
        }
    }

    fn needs_rebuild_after_update(&mut self, ctx: &mut UpdateCtx) -> bool {
        match self {
            MdPart::Paragraph { layout }
            | MdPart::Code { layout }
            | MdPart::Heading { layout, .. } => layout.needs_rebuild_after_update(ctx),
            MdPart::Blockquote { content } => {
                let mut all = false;
                for el in content {
                    all |= el.needs_rebuild_after_update(ctx);
                }
                all
            }
        }
    }

    // Ensure that the layout has been built or rebuilt as necessary before calling this.
    fn size(&self) -> Size {
        match self {
            MdPart::Paragraph { layout } => layout.size(),
            MdPart::Code { layout } => layout.size(),
            MdPart::Heading { level, layout } => layout.size(),
            MdPart::Blockquote { content } => {
                let mut height = 0.;
                let mut width: f64 = 0.;
                for part in content {
                    width = width.max(part.size().width);
                    height += part.size().height + PARA_SPACING;
                }
                Size::new(width, height)
            }
        }
    }

    fn paint(&self, ctx: &mut PaintCtx, env: &Env, pos: Point) {
        match self {
            MdPart::Paragraph { layout } => {
                layout.draw(ctx, pos);
            }
            MdPart::Code { layout } => {
                layout.draw(ctx, pos);
            }
            MdPart::Heading { level, layout } => {
                let pos = Point::new(pos.x, pos.y + PARA_SPACING);
                layout.draw(ctx, pos);
            }
            MdPart::Blockquote { content } => todo!(),
        }
    }
}

fn link_event(ctx: &mut EventCtx, event: &Event, layout: &mut TextLayout<RichText>) {
    if let Event::MouseDown(evt) = event {
        if let Some(link) = layout.link_for_pos(evt.pos) {
            ctx.submit_command(link.command.clone());
        }
    }
}

// build structure of our display
fn build_md_content<T: TextStorage>(data: &T) -> Vec<MdPart> {
    use pulldown_cmark::{Event, Options, Tag};
    let mut content = vec![];
    let parser = Parser::new_ext(data.as_str(), Options::all());
    let mut event_iter = parser.into_iter();
    while let Some(event) = event_iter.next() {
        match event {
            Event::Start(tag) => match tag {
                Tag::Paragraph => {
                    build_paragraph(&mut event_iter, &mut content);
                }
                Tag::Heading(level) => build_heading(&mut event_iter, &mut content, level),
                Tag::BlockQuote => (),
                Tag::CodeBlock(kind) => build_codeblock(&mut event_iter, &mut content, &kind),
                Tag::List(_) => (),
                Tag::Item => (),
                Tag::FootnoteDefinition(_) => (),
                Tag::Table(_) => (),
                Tag::TableHead => (),
                Tag::TableRow => (),
                Tag::TableCell => (),
                Tag::Emphasis => (),
                Tag::Strong => (),
                Tag::Strikethrough => (),
                Tag::Link(_, _, _) => (),
                Tag::Image(_, _, _) => (),
            },
            Event::Rule => todo!(),
            Event::Html(s) => tracing::error!("inline html in markdown unsupported, skipping"),
            evt => panic!("unexpected md event {:?}", evt),
        }
    }
    content
}

fn build_heading(event_iter: &mut Parser, content: &mut Vec<MdPart>, level: u32) {
    let (layout, tag) = build_text(
        event_iter,
        Some(&|attrs| add_attributes_for_header(level, attrs)),
    );
    // set heading size
    expect_tag(tag, Tag::Heading(level));
    content.push(MdPart::Paragraph { layout })
}

fn build_paragraph(event_iter: &mut Parser, content: &mut Vec<MdPart>) {
    use pulldown_cmark::{Event, Tag};

    let (layout, tag) = build_text(event_iter, None);
    expect_tag(tag, Tag::Paragraph);
    content.push(MdPart::Paragraph { layout })
}

/// Returns the text layout, and the end tag that signified the end of the enclosing context.
///
/// The `attrs_adder` allows for attributes to be applied to all the text, if desired.
fn build_text<'a>(
    event_iter: &mut Parser<'a>,
    attrs_adder: Option<&dyn Fn(AttributesAdder)>,
) -> (TextLayout<RichText>, pulldown_cmark::Tag<'a>) {
    let mut text = RichTextBuilder::new();
    let mut ctx_stack = Vec::new();
    let mut current_pos = 0;
    let mut whitespace_last = true;

    loop {
        match event_iter.next() {
            Some(ParseEvent::Start(tag)) => {
                ctx_stack.push((current_pos, tag));
            }
            Some(ParseEvent::End(end_tag)) => match ctx_stack.pop() {
                Some((start_off, tag)) => {
                    if tag == end_tag {
                        add_attribute_for_tag(
                            &tag,
                            text.add_attributes_for_range(start_off..current_pos),
                        );
                    } else {
                        panic!("expected end of {:?}, found {:?}", tag, end_tag);
                    }
                }
                None => {
                    if let Some(f) = attrs_adder {
                        f(text.add_attributes_for_range(..));
                    }
                    return (TextLayout::from_text(text.build()), end_tag);
                }
            },
            Some(ParseEvent::Text(string)) => {
                text.push(&string);
                current_pos += string.len();
                if let Some(ch) = string.chars().next_back() {
                    whitespace_last = ch.is_whitespace();
                }
            }
            Some(ParseEvent::SoftBreak) => {
                if !whitespace_last {
                    text.push(" ");
                    current_pos += 1;
                }
            }
            Some(ParseEvent::HardBreak) => {
                text.push("\n");
                current_pos += 1;
            }
            evt => panic!("unexpected event {:?}", evt),
        }
    }
}

/// Handle a codeblock, consuming tokens as needed
fn build_codeblock(event_iter: &mut Parser, content: &mut Vec<MdPart>, kind: &CodeBlockKind) {
    let mut builder = RichTextBuilder::new();
    let block_name = match kind {
        CodeBlockKind::Indented => "",
        CodeBlockKind::Fenced(ty) => *&ty,
    };
    let text = match event_iter.next().expect("expected text") {
        ParseEvent::Text(code_text) => code_text,
        ParseEvent::End(Tag::CodeBlock(_)) => {
            content.push(MdPart::Code {
                layout: TextLayout::from_text(builder.build()),
            });
            return;
        }
        evt => {
            tracing::warn!("unexpected event: {:?}", evt);
            content.push(MdPart::Code {
                layout: TextLayout::from_text(builder.build()),
            });
            return;
        }
    };
    let theme = &THEMES.themes["Solarized (dark)"];
    // Try to guess the type of data
    let syntax = SYNTAX
        // like find_syntax_by_name but ignores case
        .syntaxes()
        .iter()
        .find(|syntax| syntax.name.eq_ignore_ascii_case(block_name))
        .or_else(|| SYNTAX.find_syntax_by_extension(block_name))
        .or_else(|| SYNTAX.find_syntax_by_first_line(&text))
        .unwrap_or_else(|| SYNTAX.find_syntax_plain_text());
    let mut h = HighlightLines::new(syntax, theme);
    for line in LinesWithEndings::from(&text) {
        let ranges: Vec<(Style, &str)> = h.highlight(line, &SYNTAX);
        for (style, subtext) in ranges {
            apply_styles(builder.push(subtext), style);
        }
    }
    match event_iter.next() {
        Some(ParseEvent::End(Tag::CodeBlock(_))) => (),
        evt => tracing::warn!("unexpected token {:?}", evt),
    }
    content.push(MdPart::Code {
        layout: TextLayout::from_text(builder.build()),
    });
}

fn apply_styles(mut adder: AttributesAdder, style: Style) {
    adder.text_color(syntect_to_druid_color(style.foreground));
    if style
        .font_style
        .contains(syntect::highlighting::FontStyle::BOLD)
    {
        adder.weight(FontWeight::BOLD);
    }
    if style
        .font_style
        .contains(syntect::highlighting::FontStyle::ITALIC)
    {
        adder.style(FontStyle::Italic);
    }
}

fn syntect_to_druid_color(color: syntect::highlighting::Color) -> Color {
    Color::rgba8(color.r, color.g, color.b, color.a)
}

fn add_newline_after_tag(tag: &Tag) -> bool {
    !matches!(
        tag,
        Tag::Emphasis | Tag::Strong | Tag::Strikethrough | Tag::Link(..)
    )
}

fn add_attributes_for_header(level: u32, mut attrs: AttributesAdder) {
    let font_size = match level {
        1 => 38.,
        2 => 32.0,
        3 => 26.0,
        4 => 20.0,
        5 => 16.0,
        _ => 12.0,
    };
    attrs.size(font_size).weight(FontWeight::BOLD);
}

fn add_attribute_for_tag(tag: &Tag, mut attrs: AttributesAdder) {
    match tag {
        Tag::Heading(lvl) => {
            let font_size = match lvl {
                1 => 38.,
                2 => 32.0,
                3 => 26.0,
                4 => 20.0,
                5 => 16.0,
                _ => 12.0,
            };
            attrs.size(font_size).weight(FontWeight::BOLD);
        }
        Tag::BlockQuote => {
            attrs.style(FontStyle::Italic).text_color(BLOCKQUOTE_COLOR);
        }
        Tag::CodeBlock(CodeBlockKind::Indented) => {
            attrs.font_family(FontFamily::MONOSPACE);
        }
        Tag::CodeBlock(CodeBlockKind::Fenced(label)) => {
            attrs.font_family(FontFamily::MONOSPACE);
        }
        Tag::Emphasis => {
            attrs.style(FontStyle::Italic);
        }
        Tag::Strong => {
            attrs.weight(FontWeight::BOLD);
        }
        Tag::Link(_link_ty, target, _title) => {
            attrs
                .underline(true)
                .text_color(LINK_COLOR)
                .link(OPEN_LINK.with(target.to_string()));
        }
        // ignore other tags for now
        _ => (),
    }
}

fn expect_tag(tag: pulldown_cmark::Tag, test: pulldown_cmark::Tag) {
    if !(tag == test) {
        panic!("expected end event for {:?}, found {:?}", tag, test);
    }
}
