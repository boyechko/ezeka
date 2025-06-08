# ezeka: Eclectic Zettelkasten (ezeka.el)

This package handles primarily the content of the Zettel themselves,
leaving most of the interaction (listing, searching, etc.) to Octavo (my
fork of Grant Rosson's excellent Zk, Zk-Desktop, and Zk-Index).

## What is a Zettelkasten?

A Zettelkasten (German for "note-box" or "slip-box") is a collection of
interlinked notes (Zettel), each uniquely identified and, ideally, pertaining to
a single topic or idea. The system was developed by a prominent German
sociologist, Nicklas Luhmann (1927-1998), who credited it for how prolific he
was in publishing ground-breaking scholarship.

For more on Luhmann's Zettelkasten, see [Christian and Sascha's *Zettelkasten*
blog](https://zettelkasten.de/overview/) or [Sönke Ahrens's *How to Take Smart
Notes* (2022)](https://a.co/d/4FuqvKz). For a brief scholarly introduction, see
Johannes Schmidt's "Niklas Luhmann's Card Index: Thinking Tool, Communication
Partner, Publication Machine" (2016,
[DOI](https://doi.org/10.1163/9789004325258_014)).

There are nowadays a number of software packages that make it possible to
maintain one's own digital Zettelkasten: [Obsidian](https://obsidian.md/),
[Roam](https://roamresearch.com/), and [The
Archive](https://zettelkasten.de/the-archive/) for dedicated applications. In
Emacs itself, there is at least [Org-roam](https://www.orgroam.com/) and [Grant
Rosson's Zk](https://github.com/localauthor/zk).

## Why Eclectic Zettelkasten?

The central requirements that this package aims to fulfill are as follows:

- Implement the minimal set of features to keep a Zettelkasten in Emacs.
- Remain device- and software-agnostic by using plain text files.
- Make it convenient to maintain basic metadata inside the files themselves.
- Keep the files reasonably readable outside of Emacs.
- Organize the notes into a few broad sections (or Kästen).
- Allow convenient linking between notes in different sections.

It does this through an Emacs minor mode that maintains the integrity of the
metadata in the file's header, allows linking to other notes in the same or
different Kästen, and provides some utilities for the upkeep of the
Zettelkasten.

## Requirements

* UNIX-based operating system with standard POSIX utilities
* Filesystem that allows symbolic links
* Emacs 28.1 or higher
* org-mode
* (Optional) vertico
* (Optional) orderless
* (Optional) embark

## Installation

The package currently only exists on GitHub, so you'll need to clone the
repository, add the directory to the Emacs \`load-path\`, and then add `(require
'ezeka)` to your Emacs init file. Alternatively, you can use
[use-package](https://github.com/jwiegley/use-package) and
[straight.el](https://github.com/radian-software/straight.el). Using the latter,
the more-or-less minimal configuration is as follows:

``` elisp
(use-package ezeka
  :straight (:host github :repo "boyechko/ezeka")
  :custom
  (ezeka-directory "/path/to/Zettelkasten/")
  (ezeka-new-numerus-currens-method 'auto)
  (ezeka-save-after-metadata-updates t)
  (ezeka-make-help-echo-overlays t)
  (ezeka-genera '((?p ?p "placeholder")))
  (ezeka-placeholder-genus ?p)
  :hook
  (org-mode-hook . (lambda ()
                     (when (ezeka-file-p buffer-file-name)
                       (ezeka-mode 1))))
  (ezeka-mode-hook . ezeka--make-help-echo-overlays))
```

The \`ezeka-directory\` would contain at least one (or all three) of the
subdirectories listed in "Structure of the Zettelkasten" section below.

## Reference

### Structure of the Zettelkasten

The "note cabinet" is divided into a few different "boxes" (Kästen) that
reside in separate directories, contain different types of notes, and
have distinct ID format.

The box titles I use are as follows, but they can be changed (see
"Defining Kästen"):

- **tempus** (short for *tempus currens* \[Lat. current time\]):

  + contains fleeting notes, freewrites, journal entries, memos,
    technical notes, etc.
  + each ID is a basic ISO8601 timestamp, e.g. `20250412T2239`

- **numerus** (short for *numerus currens* \[Lat. current number\]):

  + contains literature and permanent notes, project descriptions,
    indices, etc.
  + each ID is a letter followed by a dash (-) and four digits, e.g.
    `a-1234`, à la ciphers in Zamyatin's *We* \[1921\]

- **scriptum** (Lat. for "text" or "written work"):

  + contains drafts and finished pieces of writing associated with a
    particular project
  + each ID is a *numerus currens* followed by a tilde (~) and a
    two-digit number (e.g. `a-1234~04`)

Because of the disparate nature of how IDs are generated (time of day,
random, and partly sequential, respectively), changing those requires
*(for now)* modifying some parts of \`ezeka-file.el\`.

For the rationale in separating fleeting (Ahrens's term) or engagement
notes (Sheffler's term) from permanent (Ahrens's) or memory notes
(Sheffler's), see [Dan Sheffler's "Two Goals of Note Taking"
(2014-07-21)](https://www.dtsheffler.com/notebook/2014-07-21-two-goals-of-note-taking/)
and/or chapter six of Sönke Ahrens's *How to Take Smart Notes* (2022).

### Format of the Notes

Each Zettel file begins with a header encoding metadata in a very
simplified version of [YAML](https://yaml.org/):

1.  The first line of the header contains the rubric (i.e. synopsis)
    line condensing various key pieces of information about the note.

    ``` example
    rubric: §r-8526 {χ} using metadata block in notes
    ```

    The rubric consists of the following elements:

    1.  \[Optional\] '§', signifying that differences between caption
        and title are acknowledged and should remain as they are without
        further confirmations;
    2.  Note ID ("r-8526" in the example above);
    3.  Genus (for numerus) or category (for tempus and scriptum) in
        squiggly brackets ("{χ}" above);
    4.  Note's caption, a sanitized and possibly shortened title;
    5.  A citation key if the note is about a specific work.

    The rubric (sans any '§') should match the actual filename or Ezeka
    will prompt the user to reconcile the two.

2.  The rest of the metadata follows in YAML mappings:

    Each line consists of a key (scalar) and a value (scalar or flow
    sequence). Currently recognized keys are:

    - **title:** human-Formatted Title used when inserting links
    - **created:** org-mode time stamp of note's creation time (Required)
    - **modified:** org-mode time stamp of last modification time
    - **parent:** ID of the parent note, if any
    - **firstborn:** ID of the first child note, if any
    - **oldnames:** list of IDs that used to identify the note in the past
    - **readings:** list of ISO8601 dates of when the work was consumed
    - **keywords:** list of \#-prefixed words used as keywords

    Some examples:

    ``` example
    rubric: r-8526 {χ} format of Zettel content and metadata, YAML header
    title: format of Zettel content and metadata, YAML header
    created: 2015-05-24 Sun 14:56
    modified: 2025-04-12 Sat 13:37
    parent: f-4144
    oldnames: [ 028-ada, 548-uqm, 20150524T1456 ]
    ```

    ``` example
    rubric: 20131119T1719 {Tip} splitting PDF into individual TIFF images
    title: splitting PDF into individual TIFF images
    created: 2013-11-19 Tue 17:19
    modified: 2025-04-08 Tue 12:21
    oldnames: [ 117-c ]
    ```

    ``` example
    rubric: §c-9721~01 {Presentation} Utopian Studies, 'Intentional Performativity and Impermanence' (2022-07-14)
    title: Utopian Studies Society presentation on "Intentional Performativity and Impermanence" (2022-07-14)
    created: 2022-07-05 Tue 11:50
    modified: 2022-07-18 Mon 16:08
    parent: c-9721
    ```

3.  Blank line[^1] to separate the header from the note content.

4.  Text of the note, usually consisting of:

    - brief introduction about the note's origin;
    - series of org-mode headings for relevant sections; and
    - (optional) `Change Log` heading with notable numbering and naming
      changes.

# Footnotes

[^1]: If desired, a line consisting of three dashes (`---`) can be
    placed at the beginning and the end of the "directives" section
    (i.e. the header) to make it more strictly follow YAML conventions.
    In that case, a blank line is not needed.
