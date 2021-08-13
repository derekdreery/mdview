// Copyright 2020 The Druid Authors.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! An example of live markdown preview

use std::fs;
use std::ops::Range;
use std::path::Path;
use std::sync::Arc;

use druid::commands::{COPY, CUT, NEW_FILE, OPEN_FILE, PASTE, SAVE_FILE, SAVE_FILE_AS};
use pulldown_cmark::{CodeBlockKind, Event as ParseEvent, Parser, Tag};

use druid::text::{
    AttributesAdder, EditableText, RichText, RichTextBuilder, Selection, TextStorage,
};
use druid::widget::prelude::*;
use druid::widget::{Controller, LineBreaking, RawLabel, Scroll, Split, TextBox};
use druid::{
    AppDelegate, AppLauncher, Application, ArcStr, Color, Command, Data, DelegateCtx,
    FontDescriptor, FontFamily, FontStyle, FontWeight, Handled, Lens, LocalizedString, Menu,
    Selector, Target, Widget, WidgetExt, WindowDesc, WindowId,
};
use once_cell::sync::Lazy as SyncLazy;
use syntect::easy::HighlightLines;
use syntect::highlighting::{Highlighter, Style, Theme, ThemeSet};
use syntect::parsing::{ParseState, ScopeStack, SyntaxSet};
use syntect::util::LinesWithEndings;

const WINDOW_TITLE: LocalizedString<AppState> = LocalizedString::new("Minimal Markdown");

const SPACER_SIZE: f64 = 8.0;
const BLOCKQUOTE_COLOR: Color = Color::grey8(0x88);
const LINK_COLOR: Color = Color::rgb8(0, 0, 0xEE);
const OPEN_LINK: Selector<String> = Selector::new("druid-example.open-link");
const SELECTION_CHANGE: Selector<Selection> = Selector::new("selection-change");
const CLEAR_SELECTION: Selector<()> = Selector::new("selection-clear");

static SYNTAX: SyncLazy<SyntaxSet> = SyncLazy::new(|| SyntaxSet::load_defaults_newlines());
static THEMES: SyncLazy<ThemeSet> = SyncLazy::new(|| ThemeSet::load_defaults());

#[derive(Clone, Data, Lens)]
struct AppState {
    /// `None` represents a buffer without an associated file.
    open_file: Option<Arc<Path>>,
    raw: Arc<String>,
    rendered: RichText,
    #[data(eq)]
    selection: Selection,
}

impl AppState {
    fn selection_as_range(&self) -> Range<usize> {
        if self.selection.anchor <= self.selection.active {
            self.selection.anchor..self.selection.active
        } else {
            self.selection.active..self.selection.anchor
        }
    }
}

/// A controller that rebuilds the preview when edits occur
struct RichTextRebuilder;

impl<W: Widget<AppState>> Controller<AppState, W> for RichTextRebuilder {
    fn event(
        &mut self,
        child: &mut W,
        ctx: &mut EventCtx,
        event: &Event,
        data: &mut AppState,
        env: &Env,
    ) {
        let pre_data = data.raw.clone();
        child.event(ctx, event, data, env);
        if !data.raw.same(&pre_data) {
            data.rendered = rebuild_rendered_text(&data.raw);
        }
    }
}

#[derive(Default)]
struct SelectionNotifier {
    selection: Selection,
}

impl SelectionNotifier {
    fn new() -> Self {
        Default::default()
    }
}

impl<T> Controller<T, TextBox<T>> for SelectionNotifier
where
    T: TextStorage + EditableText,
{
    fn event(
        &mut self,
        child: &mut TextBox<T>,
        ctx: &mut EventCtx,
        event: &Event,
        data: &mut T,
        env: &Env,
    ) {
        struct SelectionEq(Selection);
        impl SelectionEq {
            fn is_empty(&self) -> bool {
                self.0.anchor == self.0.active
            }
        }
        impl PartialEq for SelectionEq {
            fn eq(&self, other: &Self) -> bool {
                (self.0.anchor == other.0.anchor && self.0.active == other.0.active)
                    || (self.0.anchor == other.0.active && self.0.active == other.0.anchor)
            }
        }
        child.event(ctx, event, data, env);
        if matches!(event, Event::Command(cmd) if cmd.is(CLEAR_SELECTION)) {
            let mut selection = child.text().borrow().selection();
            selection.active = selection.anchor;
            if let Some(i) = child.text().borrow_mut().set_selection(selection) {
                ctx.invalidate_text_input(i);
            }
        }
        let current_selection = child.text().borrow().selection();
        if SelectionEq(self.selection) != SelectionEq(current_selection) {
            self.selection = current_selection;
            ctx.submit_command(SELECTION_CHANGE.with(self.selection));
        }
    }
}

struct Delegate;

impl AppDelegate<AppState> for Delegate {
    fn command(
        &mut self,
        ctx: &mut DelegateCtx,
        _target: Target,
        cmd: &Command,
        data: &mut AppState,
        _env: &Env,
    ) -> Handled {
        if let Some(url) = cmd.get(OPEN_LINK) {
            #[cfg(not(target_arch = "wasm32"))]
            open::that_in_background(url);
            #[cfg(target_arch = "wasm32")]
            tracing::warn!("opening link({}) not supported on web yet.", url);
            Handled::Yes
        } else if cmd.get(NEW_FILE).is_some() {
            data.open_file = None;
            data.raw = "".to_string().into();
            data.rendered = rebuild_rendered_text(&data.raw);
            Handled::Yes
        } else if let Some(finfo) = cmd.get(OPEN_FILE) {
            let file_contents = match fs::read_to_string(finfo.path()) {
                Ok(s) => s,
                Err(e) => {
                    tracing::error!("Failed to open file {}: {}", finfo.path().display(), e);
                    return Handled::Yes;
                }
            };
            data.open_file = Some(finfo.path().to_owned().into());
            data.raw = file_contents.into();
            data.rendered = rebuild_rendered_text(&data.raw);
            Handled::Yes
        } else if cmd.get(SAVE_FILE).is_some() {
            if let Some(path) = &data.open_file {
                if let Err(e) = fs::write(path, &*data.raw) {
                    tracing::error!("Failed to save file: {}", e);
                }
            } else {
                tracing::error!("Save without filename should not be possible");
            }
            Handled::Yes
        } else if let Some(finfo) = cmd.get(SAVE_FILE_AS) {
            match fs::write(finfo.path(), &*data.raw) {
                Ok(()) => {
                    data.open_file = Some(finfo.path().to_owned().into());
                }
                Err(e) => {
                    tracing::error!("Could not save file: {}", e);
                    data.open_file = None;
                }
            }
            Handled::Yes
        } else if let Some(selection) = cmd.get(SELECTION_CHANGE) {
            data.selection = *selection;
            tracing::trace!("selection changed: {:?}", data.selection);
            Handled::Yes
        } else if cmd.get(CUT).is_some() {
            let range = data.selection_as_range();
            Application::global()
                .clipboard()
                .put_string(&data.raw[range.clone()]);
            Arc::make_mut(&mut data.raw).replace_range(range, "");
            ctx.submit_command(CLEAR_SELECTION);
            Handled::Yes
        } else if cmd.is(COPY) {
            let range = data.selection_as_range();
            Application::global()
                .clipboard()
                .put_string(&data.raw[range.clone()]);
            Handled::Yes
        } else if cmd.is(PASTE) {
            let range = data.selection_as_range();
            let replacement = Application::global()
                .clipboard()
                .get_string()
                .unwrap_or("".into());
            Arc::make_mut(&mut data.raw).replace_range(range, &replacement);
            Handled::Yes
        } else {
            Handled::No
        }
    }
}

pub fn main() {
    log_to_console();

    // describe the main window
    let main_window = WindowDesc::new(build_root_widget())
        .title(WINDOW_TITLE)
        .menu(make_menu)
        .window_size((700.0, 600.0));

    // create the initial app state
    let initial_state = AppState {
        open_file: None,
        raw: "".to_string().into(),
        rendered: rebuild_rendered_text(""),
        selection: Selection::new(0, 0),
    };

    tracing::debug!("Themes: {:?}", THEMES.themes.keys().collect::<Vec<_>>());
    tracing::trace!("Syntaxes:",);
    for syntax in SYNTAX.syntaxes().iter() {
        tracing::trace!("  {}: {:?}", syntax.name, syntax.file_extensions);
    }

    // start the application
    AppLauncher::with_window(main_window)
        .delegate(Delegate)
        .launch(initial_state)
        .expect("Failed to launch application");
}

fn log_to_console() {
    use tracing_subscriber::prelude::*;
    let filter_layer = tracing_subscriber::filter::LevelFilter::DEBUG;
    let fmt_layer = tracing_subscriber::fmt::layer()
        // Display target (eg "my_crate::some_mod::submod") with logs
        .with_target(true);

    tracing_subscriber::registry()
        .with(filter_layer)
        .with(fmt_layer)
        .init();
}

fn build_root_widget() -> impl Widget<AppState> {
    let label = Scroll::new(
        RawLabel::new()
            .with_text_color(Color::BLACK)
            .with_line_break_mode(LineBreaking::WordWrap)
            .lens(AppState::rendered)
            .expand_width()
            .padding((SPACER_SIZE * 4.0, SPACER_SIZE)),
    )
    .vertical()
    .background(Color::grey8(222))
    .expand();

    let textbox = TextBox::multiline()
        .with_font(FontDescriptor::new(FontFamily::MONOSPACE).with_size(14.))
        .with_placeholder("Write markdown here")
        .controller(SelectionNotifier::new())
        .lens(AppState::raw)
        .expand()
        .padding(5.0);

    Split::columns(textbox, label).controller(RichTextRebuilder)
}

/// Parse a markdown string and generate a `RichText` object with
/// the appropriate attributes.
fn rebuild_rendered_text(text: &str) -> RichText {
    let mut current_pos = 0;
    let mut builder = RichTextBuilder::new();
    let mut tag_stack = Vec::new();

    let parser = Parser::new(text);
    let mut event_iter = parser.into_iter();
    while let Some(event) = event_iter.next() {
        match event {
            ParseEvent::Start(tag) => match &tag {
                Tag::CodeBlock(code_type) => {
                    handle_codeblock(&mut builder, &mut event_iter, code_type)
                }
                _ => {
                    tag_stack.push((current_pos, tag));
                }
            },
            ParseEvent::Text(txt) => {
                builder.push(&txt);
                current_pos += txt.len();
            }
            ParseEvent::End(end_tag) => {
                let (start_off, tag) = tag_stack
                    .pop()
                    .expect("parser does not return unbalanced tags");
                assert_eq!(end_tag, tag, "mismatched tags?");
                add_attribute_for_tag(
                    &tag,
                    builder.add_attributes_for_range(start_off..current_pos),
                );
                if add_newline_after_tag(&tag) {
                    builder.push("\n\n");
                    current_pos += 2;
                }
            }
            ParseEvent::Code(txt) => {
                builder.push(&txt).font_family(FontFamily::MONOSPACE);
                current_pos += txt.len();
            }
            ParseEvent::Html(txt) => {
                builder
                    .push(&txt)
                    .font_family(FontFamily::MONOSPACE)
                    .text_color(BLOCKQUOTE_COLOR);
                current_pos += txt.len();
            }
            ParseEvent::HardBreak => {
                builder.push("\n\n");
                current_pos += 2;
            }
            _ => (),
        }
    }
    builder.build()
}

/// Handle a codeblock, consuming tokens as needed
fn handle_codeblock(
    builder: &mut RichTextBuilder,
    event_iter: &mut Parser,
    code_type: &CodeBlockKind,
) {
    let block_name = match code_type {
        CodeBlockKind::Indented => "",
        CodeBlockKind::Fenced(ty) => *&ty,
    };
    let text = match event_iter.next().expect("expected text") {
        ParseEvent::Text(code_text) => code_text,
        ParseEvent::End(Tag::CodeBlock(_)) => return,
        evt => {
            tracing::warn!("unexpected event: {:?}", evt);
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

#[allow(unused_assignments, unused_mut)]
fn make_menu(_window_id: Option<WindowId>, _data: &AppState, _env: &Env) -> Menu<AppState> {
    let mut base = Menu::empty();
    #[cfg(target_os = "macos")]
    {
        base = base.entry(druid::platform_menus::mac::application::default())
    }
    #[cfg(any(target_os = "windows", target_os = "linux"))]
    {
        //base = base.entry(druid::platform_menus::win::file::default());
        base = base.entry(
            Menu::new(LocalizedString::new("common-menu-file-menu"))
                .entry(druid::platform_menus::win::file::new())
                .entry(druid::platform_menus::win::file::open())
                .entry(
                    druid::platform_menus::win::file::save()
                        .enabled_if(|data: &AppState, _| data.open_file.is_some()),
                )
                .entry(druid::platform_menus::win::file::save_as()),
        );
    }
    base.entry(
        Menu::new(LocalizedString::new("common-menu-edit-menu"))
            .entry(druid::platform_menus::common::undo())
            .entry(druid::platform_menus::common::redo())
            .separator()
            .entry(druid::platform_menus::common::cut())
            .entry(druid::platform_menus::common::copy())
            .entry(druid::platform_menus::common::paste()),
    )
}
