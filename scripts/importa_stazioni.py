#!/usr/bin/env python3
"""Pubblica «Stazioni dei Viandanti» dall'ultimo commit del repository del libro.

Legge `in/stazioni.md` (già arricchito dei versetti da linkme.py) e `metadata.yml`
dall'ultimo commit (HEAD) di ~/prj/stazioni: le modifiche non committate non
vengono pubblicate. Scrive mica/poly/it/2024-07-24-stazioni.md, mantenendo
l'indirizzo della pagina già online.

Adattamenti per il web:
  - i blocchi LaTeX (\\frontmatter, \\chapter*{...}, \\mainmatter) diventano titoli
    normali o spariscono;
  - i livelli dei titoli scendono di uno (titolo del libro, Parti, Stazioni);
  - indice iniziale delle Parti e, in ogni Parte, l'elenco delle stazioni
    diventa un elenco di collegamenti;
  - riga con la versione (data e commit) e, se impostato, il link a Lulu.

Uso:
  python3 scripts/importa_stazioni.py [~/prj/stazioni]
"""
import re
import subprocess
import sys
import unicodedata
from datetime import datetime
from pathlib import Path

import yaml

# Quando l'edizione cartacea sarà disponibile, inserire qui il link a Lulu.
LULU_URL = ""

REPO = Path(__file__).resolve().parent.parent
DST = REPO / "mica" / "poly" / "it" / "2024-07-24-stazioni.md"
CREATED = "2024-07-24"  # data della prima pubblicazione: resta quella
MESI = ["gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio",
        "agosto", "settembre", "ottobre", "novembre", "dicembre"]


def git(libro, *args):
    return subprocess.run(["git", "-C", str(libro), *args], check=True,
                          capture_output=True, text=True).stdout


def slug(s):
    s = re.sub(r"[*_]", "", s)
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    return re.sub(r"[^a-z0-9]+", "-", s.lower()).strip("-")


def adatta(testo):
    """Converte il manoscritto (pensato per LaTeX) in markdown per il sito."""
    righe, out, i = testo.split("\n"), [], 0
    parti = []  # (titolo, id, [(titolo stazione, id)])
    while i < len(righe):
        r = righe[i]
        if r.strip() == "```{=latex}":  # blocco LaTeX: tieni solo i titoli
            j = i + 1
            while righe[j].strip() != "```":
                m = re.match(r"\\chapter\*\{(.+)\}", righe[j].strip())
                if m:
                    out.append(f"## {m.group(1)} {{#{slug(m.group(1))}}}")
                j += 1
            i = j + 1
            continue
        if r.startswith(":::") and out and out[-1].strip() and not out[-1].startswith(":::"):
            out.append("")  # un div senza riga vuota prima verrebbe letto come testo
        m = re.match(r"^(#{1,2}) (.+?)\s*$", r)
        if m:
            titolo = m.group(2)
            ident = slug(titolo)
            if m.group(1) == "#":
                if parti:
                    out += ['<p class="text-align-right"><a href="#indice">↑ Indice</a></p>', ""]
                parti.append((titolo, ident, []))
            else:
                if parti:
                    ident = f"{parti[-1][1]}-{slug(titolo)}"
                    parti[-1][2].append((titolo, ident))
            out.append(f"{m.group(1)}# {titolo} {{#{ident}}}")
        else:
            out.append(r)
        i += 1
    return "\n".join(out), parti


def collega_elenchi(testo, parti):
    """In ogni Parte, l'elenco numerato iniziale punta alle sue stazioni."""
    avvisi = []
    for titolo, ident, stazioni in parti:
        inizio = testo.index(f"## {titolo} {{#{ident}}}")
        fine = testo.index("\n### ", inizio)
        blocco = testo[inizio:fine]
        voci = re.findall(r"^(\d+)\. (.+)$", blocco, re.M)
        if len(voci) != len(stazioni):
            avvisi.append(f"{titolo}: {len(voci)} voci nell'elenco ma {len(stazioni)} stazioni")
        for (n, voce), (nome, sid) in zip(voci, stazioni):
            if voce.strip().lower() != nome.lower():
                avvisi.append(f"{titolo}: elenco «{voce}» ≠ titolo «{nome}»")
            blocco = blocco.replace(f"{n}. {voce}", f"{n}. [{voce}](#{sid})", 1)
        testo = testo[:inizio] + blocco + testo[fine:]
    return testo, avvisi


def main():
    libro = Path(sys.argv[1] if len(sys.argv) > 1 else "~/prj/stazioni").expanduser()

    t_src = int(git(libro, "log", "-1", "--format=%ct", "--", "stazioni.md") or 0)
    t_in = int(git(libro, "log", "-1", "--format=%ct", "--", "in/stazioni.md") or 0)
    if t_src > t_in:
        sys.exit("stazioni.md è stato committato dopo in/stazioni.md: rilancia "
                 "`python3 linkme.py`, committa e riprova.")
    if git(libro, "status", "--porcelain", "--", "stazioni.md", "in/stazioni.md", "metadata.yml"):
        print("Nota: ci sono modifiche non committate nel libro; si pubblica l'ultimo commit.")

    commit = git(libro, "log", "-1", "--format=%h %cI").split()
    data = datetime.fromisoformat(commit[1])
    meta = yaml.safe_load(git(libro, "show", "HEAD:metadata.yml"))
    testo = git(libro, "show", "HEAD:in/stazioni.md")

    corpo, parti = adatta(testo)
    corpo, avvisi = collega_elenchi(corpo, parti)

    autore = ", ".join(meta.get("author") or [])
    indice = ["- [Nota del traduttore](#nota-del-traduttore)", "- [Proemio](#proemio)"]
    indice += [f"- [{t}](#{i})" for t, i, _ in parti]
    versione = f"Versione del {data.day} {MESI[data.month - 1]} {data.year} (revisione {commit[0]})."
    if LULU_URL:
        versione += f" Disponibile anche in [edizione cartacea su Lulu]({LULU_URL})."

    pagina = "\n".join([
        "---",
        f'title: "{meta["title"]}"',
        f"created: {CREATED}",
        "---",
        "",
        '<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Amiri&display=swap" />',
        "",
        f"# {meta['title']}",
        "",
        f"*{autore}*",
        "",
        f"{meta.get('subtitle', '')}",
        "",
        f"<small>{versione}</small>",
        "",
        "## Indice {#indice}",
        "",
        *indice,
        "",
        corpo.strip(),
        "",
    ])
    pagina = re.sub(r"\n{3,}", "\n\n", pagina)

    vecchia = DST.read_text(encoding="utf-8") if DST.exists() else ""
    if pagina != vecchia:
        DST.write_text(pagina, encoding="utf-8")
        print(f"Aggiornato {DST.relative_to(REPO)} dal commit {commit[0]} ({data:%Y-%m-%d})")
    else:
        print("Già aggiornato, nessuna modifica.")
    print(f"{len(parti)} parti, {sum(len(s) for _, _, s in parti)} stazioni")
    for a in avvisi:
        print("  attenzione:", a)


if __name__ == "__main__":
    main()
