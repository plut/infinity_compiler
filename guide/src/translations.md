# Translations

## Language identifiers

Languages are represented by their 5-letter name (as in `"en_US"`)
or, for female variants, by their 6-letter name (as in `"fr_FRF"`).

Inside the database, female variants are handled as
distinct languages from male variants;
however a number of rules exist 

(The 5- or 6-letter language identifier is lossless from the game's
`"dialog.tlk"` path; this simplifies backups etc.)

## Translations

String translation is handled through the use of the `translations_X`
table, where `X` is a 5- or 6- language identifier.
This table contains data similar to the game's `.tlk` file,
except that it is indexed by native strings instead of strrefs.

Any native string absent from this dictionary is left untranslated;
only its markers (see below) are discarded.
This is intended as a sane default value where the player,
when a string has not (yet) been translated, will see the string in the
original language, which is most often English.

## Markers

Native strings can contain markers of the following form: `{?text}`.
Such markers are discarded when a native string is used as the default
value for a game string in the absence of an appropriate translation.

These markers, however, are seen by translators when translating the
native string to another game language; most tools will even highlight
them (these look like Python formatting parameters).
It is thus **strongly recommended** to include markers in native strings
to signal any possible grammatical ambiguity
including at least the following cases:
 - distinguishing verbs from nouns etc.: `"{?verb}guard"` vs.
	 `"{?noun}guard";
 - marking grammatical gender where it is not obvious:
   e.g. `"come here, my dear {?male}friend"`.

Keep in mind that translators, when translating a string,
will generally not have access to context beyond the string itself.
Make liberal use of markers to help them produce quality work.

## Gendered languages

When producing translations for gendered languages,
the general case is that a single translation will be produced
for both gender variants.

### Gender markers
The empty marker `{?}` is a special case.
Including this marker in a native string marks this string as
needing two translations for gendered languages.
This means that, while a single native string is present in the source file,
the translator will be prompted to translate two strings,
where the empty marker will be replaced by either `{?M}` or `{?F}`.

This marker has no special meaning for non-gendered languages;
in this case the translator will see only a single string,
still bearing its empty marker.

## Conversion to strref

Conversion between native strings and strref is performed iby the
`strref_dict` table in a way similar to the `resref_dict` table.
The algorithm for generating new strrefs is of course different (the
lowest available value is used).


