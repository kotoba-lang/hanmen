# hanmen（版面）

**A page, as placed marks.** 版面 is the composed area of a printed page — the
type and rules as they sit on the paper, not the text that went into them. This
library is that value: a size, a rotation and a vector of marks with
coordinates, plus one producer (PDF) and one drawer (SVG).

Portable `.cljc`, one dependency (`org-iso-pdf`), no host effects. Reading a
file is the caller's capability to spend; nothing here opens anything.

```clojure
(require '[hanmen.pdf :as hpdf] '[hanmen.svg :as svg])

(def page (hpdf/page-at (pdf.core/parse bytes) 0))
(svg/->svg page {:fit-to [900 1200]})   ;; => "<svg …>" — inert, themeable
```

## Why this exists

`org-iso-pdf` answers what is *in* a document. `pdf.core/extract-text` returns
the strings shown between `BT` and `ET` — enough to search, and nothing to look
at, because it has no text matrix. A viewer is entirely a question of *where*.

`kotoba-lang/app-preview` had the other half: a catalog of pages with sizes,
rotations and text counts. Between "what a page weighs" and "what a page says"
there was no value describing what a page *looks like*, so there was nothing to
draw.

## Three decisions

**The coordinate space is the reader's.** PDF puts the origin at the bottom
left with y up; SVG, Canvas and every raster buffer put it at the top left with
y down. The flip happens once, here, because a viewer that flips is a viewer
that can flip twice — and an upside-down page is a bug only a human eye catches.
Rotation is applied for the same reason: `:page/width` is the width **as seen**.

**A run's start is exact and its width is optional.** Where text begins comes
out of the matrices and is arithmetic. How wide it is depends on the font's
`/Widths`, which a document only sometimes ships. When they are there the run
carries its measured width and a drawer can make its own font occupy exactly
that space; when they are not, the width is **absent rather than estimated** —
an estimate would be indistinguishable from a measurement downstream and wrong
by a different amount on every document.

**What cannot be drawn is placed and marked, never skipped.** An image, a form
XObject, a `/Type0` run with no `/ToUnicode`: each becomes a `:frame` with a
label and a reason. A page that quietly omits a mark looks exactly like a page
that is complete, and `hanmen.svg/emit-item` throws on an unknown kind rather
than dropping it, so a fourth item kind cannot be added on one side only.

## The SVG is safe inside `default-src 'none'`

Hosts that serve uploaded bytes keep SVG out of their inline allowlist on
purpose — an SVG is XML, it may carry `<script>`, and a browser runs it when it
is a document. This emitter is the other thing wearing that extension: markup
generated from a closed vocabulary of three item kinds, where document content
only ever becomes an escaped text node or an escaped attribute *value*. There
is no path from a file to an element name.

`serialize` enforces that a second time — an element outside `allowed-elements`
or an attribute outside `allowed-attributes` throws instead of being written.
No element here takes a URL, so the fragment loads nothing and the host's CSP
does not have to be widened to show a page. Deciding what a page may load is a
decision on its own, not a side effect of adding a viewer.

## It picks no colour and no font

Everything paints in `currentColor` under a class. The host's tokens decide
what the ink looks like, which is the only way one rendering is right in both
light and dark — a document's black is not the reader's black on a dark page.
Ink *density* survives (a 60% grey rule stays lighter than a black one, through
`fill-opacity`), because that difference is the document's meaning and the
absolute colour is not. Add `hanmen.svg/stylesheet` to the host's stylesheet.

## What it does not do

- **No raster.** Image XObjects are placed as frames, not decoded. Drawing them
  means a `data:` URI, which is a *load*, which is a CSP decision the host owns.
- **No reading order.** `text-of` is content-stream order — the order the
  producer emitted marks, which is close enough to reading order to search and
  nowhere near it to quote. Two-column papers come out interleaved.
- **No paths, shading or transparency groups.** Only `re … f` becomes a rule.
- **No zoom or pan.** `fit` answers the one number a server-side render needs.
  Viewport gestures are `kotoba-lang/canvaskit`'s subject.
- **No writing.** A viewer that can rewrite what it is showing is a different
  program.

## Test

```sh
clojure -M:local:test   # sibling checkouts
clojure -M:test         # pinned git deps
clojure -M:lint
```

37 tests / 121 assertions. Every placement assertion is a coordinate against a
PDF the test wrote, not a rendering somebody looked at.
