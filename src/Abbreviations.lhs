Thanks to Yann Esposito (https://github.com/yogsototh/yblog.git)

Abbreviations
=============

Declare abbreviations to be used in your content.
For example "%tldr" will be transformed in "Too long; didn't read".

You simply have to add `abbreviationFilter` has a pre-filter in the
rules (see `site.hs`).

> module Abbreviations
>   (abbreviationFilter)
> where
> import           Data.Map       (Map)
> import qualified Data.Map    as  M
> import           Data.Maybe     (fromMaybe)
> import           Hakyll

The `abbreviationFilter` make all the work, given the content it returns
the filtered one.

> abbreviationFilter :: String -> String
> abbreviationFilter = replaceAll "%[a-zA-Z0-9_]*" newnaming
>   where
>     newnaming matched = fromMaybe matched (M.lookup (tail matched) abbreviations)

All the abbreviations are declared here:

> abbreviations :: Map String String
> abbreviations = M.fromList
>     [ ("tldr",    "<span class=\"sc\"><abbr title=\"Too long; didn't read\">tl;dr</abbr>: </span>")
>     , ("tlnl",    "<span class=\"sc\"><abbr title=\"Troppo lungo; Non leggere\">tlnl</abbr>: </span>")
>     , ("imho",    "<span class=\"sc\"><abbr title=\"In my Humble Opinion\">imho</abbr></span>")
>     , ("url",     "<span class=\"sc\"><abbr title=\"Uniform Ressource Locator\">url</abbr></span>")
>     , ("rss",     "<span class=\"sc\"><abbr title=\"Rich Site Summary\">rss</abbr></span>")
>     , ("latex",   "<span style=\"text-transform: uppercase\">L<sup style=\"vertical-align: 0.15em; margin-left: -0.36em; margin-right: -0.15em; font-size: .85em\">a</sup>T<sub style=\"vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em\">e</sub>X</span>")
>     , ("xelatex", "<span style=\"text-transform: uppercase\">X<sub style=\"vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em\">&#x018E;</sub>L<sup style=\"vertical-align: 0.15em; margin-left: -0.36em; margin-right: -0.15em; font-size: .85em\">a</sup>T<sub style=\"vertical-align: -0.5ex; margin-left: -0.1667em; margin-right: -0.125em; font-size: 1em\">e</sub>X</span>")
>     , ("mymail",  "<a href=\"&#109;&#97;&#105;&#108;&#116;&#111;&#58;&#103;&#105;&#97;&#99;&#111;&#109;&#111;&#46;&#109;&#97;&#110;&#116;&#97;&#110;&#105;&#64;&#103;&#109;&#97;&#105;&#108;&#46;&#99;&#111;&#109;\">&#103;&#105;&#97;&#99;&#111;&#109;&#111;&#46;&#109;&#97;&#110;&#116;&#97;&#110;&#105;&#64;&#103;&#109;&#97;&#105;&#108;&#46;&#99;&#111;&#109;</a>")
>     ]
