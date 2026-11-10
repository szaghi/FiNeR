"""Draw the diagrams of the documentation: docs/diagrams/*.svg, inlined in the pages with <!--@include: -->.

Usage: python3 scripts/docs_diagrams.py

The diagrams are drawn with the CSS classes of docs/.vitepress/theme/custom.css (`.finer-diagram`, `d-*`), which take
their colours from the theme tokens: the same SVG reads in the light and in the dark theme. Their animations are CSS
too (`d-reveal`, `d-pulse`, with the step in the custom property `--i`), so they stop for a reader who asks for reduced
motion. Each SVG is written on lines with no blank line among them, so that the Markdown include keeps it one HTML
block. Regenerate them after changing this script; never edit the SVG files by hand.
"""

from __future__ import annotations

from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / 'docs' / 'diagrams'
CHAR = 7.5  # width of a character of the mono font at 12.5px (px)


def esc(text: str) -> str:
    """Escape the XML special characters of a text."""
    return text.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;').replace('"', '&quot;')


class Svg:
    """A minimal SVG writer: elements appended in order, one per line."""

    def __init__(self, width: int, height: int, label: str) -> None:
        self.lines = [f'<svg class="finer-diagram" viewBox="0 0 {width} {height}" role="img" '
                      f'aria-label="{esc(label)}" xml:space="preserve" xmlns="http://www.w3.org/2000/svg">']

    def add(self, element: str) -> None:
        self.lines.append(element)

    def text(self, x: float, y: float, text: str, cls: str = '', anchor: str = 'start', step: int | None = None) -> None:
        style = f' style="--i:{step}"' if step is not None else ''
        cls = f' class="{cls}"' if cls else ''
        self.add(f'<text x="{x:.1f}" y="{y:.1f}" text-anchor="{anchor}"{cls}{style}>{esc(text)}</text>')

    def rect(self, x: float, y: float, w: float, h: float, cls: str, rx: float = 6, step: int | None = None) -> None:
        style = f' style="--i:{step}"' if step is not None else ''
        self.add(f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" rx="{rx}" class="{cls}"{style}/>')

    def line(self, x1: float, y1: float, x2: float, y2: float, cls: str, step: int | None = None) -> None:
        style = f' style="--i:{step}"' if step is not None else ''
        self.add(f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" class="{cls}"{style}/>')

    def save(self, name: str) -> None:
        OUT.mkdir(parents=True, exist_ok=True)
        (OUT / name).write_text('\n'.join(self.lines + ['</svg>']) + '\n')


def anatomy() -> None:
    """An INI file, line by line, with what FiNeR makes of each line; the lines appear one after the other."""
    # (pieces of the line as (text, class), the note on the right)
    rows = [
        ([('; the case of a heat run', 'd-comment')], 'comment line: skipped'),
        ([('title', 'd-name'), (' = ', 'd-sep'), ('cooling of a copper rod', 'd-value')],
         'before any header: global section ""'),
        ([('[grid]', 'd-section')], 'section header: a new section'),
        ([('cells', 'd-name'), ('  = ', 'd-sep'), ('64', 'd-value')], 'option: name, separator, value'),
        ([('length', 'd-name'), (' = ', 'd-sep'), ('0.5', 'd-value'), ('          ; metres', 'd-comment')],
         'inline comment after ";": kept apart'),
        ([('origin', 'd-name'), (' = ', 'd-sep'), ('0.0 0.0 0.0', 'd-value')], 'a list: values split by blanks'),
        ([('[boundaries]', 'd-section')], 'another section'),
        ([('probes', 'd-name'), (' = ', 'd-sep'), ('0.1', 'd-value')], 'a value on several lines...'),
        ([('         ', 'd-sep'), ('0.25', 'd-value')], '...a line without "=" continues it'),
        ([('         ', 'd-sep'), ('0.4', 'd-value')], 'probes = 0.1 0.25 0.4'),
    ]
    top, row, left, notes = 46, 30, 26, 300
    width, height = 680, top + row * len(rows) + 20
    svg = Svg(width, height, 'The anatomy of an INI file: comment lines, the global section, section headers, '
                             'options with name, separator and value, inline comments, lists and continuation lines')
    svg.rect(10, 10, width - 20, height - 20, 'd-frame', rx=10)
    svg.text(left, 34, 'heat.ini', 'd-title')
    svg.text(notes, 34, 'what FiNeR reads', 'd-title d-muted')
    for i, (pieces, note) in enumerate(rows):
        y = top + 14 + row * i
        svg.rect(left - 8, y - 18, width - 2 * left + 16, row - 4, 'd-band d-reveal', rx=5, step=i)
        x = left
        for text, cls in pieces:
            svg.text(x, y, text, f'd-mono {cls} d-reveal', step=i)
            x += CHAR * len(text)
        svg.line(min(x + 8, notes - 30), y - 4, notes - 12, y - 4, 'd-leader d-reveal', step=i)
        svg.text(notes, y, note, 'd-note d-reveal', step=i)
    svg.save('anatomy.svg')


def model() -> None:
    """The data model: a file_ini holds sections, a section holds options, an option a name, values and comment."""
    width, height = 680, 250
    svg = Svg(width, height, 'The data model: a file_ini holds an array of sections, each section an array of '
                             'options, each option a name, its values and an inline comment')
    boxes = [
        (14, 'file_ini', ['filename (public)', 'sections(:)', 'separator for load'],
         ['load, save, print', 'get, get_string, add, del', 'loop, index, has_*']),
        (242, 'section', ['name ("" = global)', 'options(:)'], ['one per [header]', 'in file order,', 'the global one first']),
        (470, 'option', ['name', 'values (text)', 'inline comment'], ['converted on get,', 'formatted on add']),
    ]
    for k, (x, title, fields, notes) in enumerate(boxes):
        svg.rect(x, 30, 196, 190, 'd-box d-reveal', rx=10, step=k)
        svg.rect(x, 30, 196, 40, 'd-seal-soft d-reveal', rx=10, step=k)
        svg.text(x + 98, 56, title, 'd-title d-mono-title d-reveal', anchor='middle', step=k)
        for j, field in enumerate(fields):
            svg.text(x + 18, 96 + 22 * j, field, 'd-mono d-reveal', step=k)
        for j, note in enumerate(notes):
            svg.text(x + 18, 168 + 18 * j, note, 'd-note d-reveal', step=k)
        if k < len(boxes) - 1:
            svg.line(x + 198, 106, x + 220, 106, 'd-link d-reveal', step=k + 1)
            svg.add(f'<path d="M{x + 220:.1f},101 L{x + 228:.1f},106 L{x + 220:.1f},111 Z" '
                    f'class="d-arrowhead d-reveal" style="--i:{k + 1}"/>')
            svg.text(x + 214, 96, '1..n', 'd-note', anchor='middle')
    svg.save('model.svg')


def pipeline() -> None:
    """What load does to a source, stage by stage; each stage lights up in turn."""
    stages = [
        ('source', 'a file or a string'),
        ('lines', 'CR dropped, split on LF'),
        ('comments', '! ; # lines skipped'),
        ('sections', 'split at [headers]'),
        ('continuations', 'joined to their option'),
        ('options', 'name = value ; comment'),
    ]
    width, height = 680, 230
    svg = Svg(width, height, 'What load does: the source is split into lines (carriage returns dropped), comment '
                             'lines are skipped, the lines are grouped by section header, continuation lines are '
                             'joined to their option, and each option is split into name, value and inline comment')
    w, h, gap = 196, 64, 36
    for k, (title, note) in enumerate(stages):
        row, col = divmod(k, 3)
        x, y = 14 + col * (w + gap), 20 + row * (h + 46)
        svg.rect(x, y, w, h, 'd-box d-pulse', rx=8, step=k)
        svg.text(x + w / 2, y + 27, title, 'd-title', anchor='middle')
        svg.text(x + w / 2, y + 48, note, 'd-note', anchor='middle')
        if col < 2 and k < len(stages) - 1:  # to the next stage on the same row
            svg.line(x + w + 4, y + h / 2, x + w + gap - 6, y + h / 2, 'd-link')
            svg.add(f'<path d="M{x + w + gap - 8:.1f},{y + h / 2 - 4} L{x + w + gap - 2:.1f},{y + h / 2} '
                    f'L{x + w + gap - 8:.1f},{y + h / 2 + 4} Z" class="d-arrowhead"/>')
    # from the end of the first row down to the start of the second
    x1, y1 = 14 + 2 * (w + gap) + w / 2, 20 + h + 4
    x2, y2 = 14 + w / 2, 20 + h + 46 - 6
    ym = (y1 + y2) / 2
    svg.add(f'<path d="M{x1:.1f},{y1} L{x1:.1f},{ym} L{x2:.1f},{ym} L{x2:.1f},{y2 - 4}" class="d-link" fill="none"/>')
    svg.add(f'<path d="M{x2 - 4:.1f},{y2 - 6} L{x2:.1f},{y2} L{x2 + 4:.1f},{y2 - 6} Z" class="d-arrowhead"/>')
    svg.text(width / 2, 218, 'the result replaces the sections of the file_ini; values stay text until get converts them',
             'd-note', anchor='middle')
    svg.save('pipeline.svg')


if __name__ == '__main__':
    anatomy()
    model()
    pipeline()
    print(f'docs_diagrams: {len(list(OUT.glob("*.svg")))} diagrams in {OUT.relative_to(ROOT)}')
