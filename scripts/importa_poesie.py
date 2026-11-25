#!/usr/bin/env python3
"""Importa le poesie da ~/prj/poesie nella sezione "Poesie e riflessioni" del sito.

Struttura attesa della sorgente: una cartella per data (AAAAMMGG) con uno o più
file .md (titolo nella prima riga "# Titolo") e le immagini usate.

Per ogni testo lo script:
  - scrive mica/poesie/it/AAAA-MM-GG-<slug>.md con title/created nell'intestazione;
  - trasforma i link alle immagini locali "[](./foto.png)" in immagini vere,
    convertite in webp (max 1200px di larghezza) in static/img/poesie/;
  - trasforma le righe rientrate di 4 spazi (citazioni) in blockquote.

Le immagini non usate da nessun testo non vengono pubblicate. I file sorgente non
vengono modificati. Si può rilanciare quando si vuole: riscrive solo ciò che è cambiato.

Uso:
  python3 scripts/importa_poesie.py [~/prj/poesie]
"""
import json
import re
import sys
import unicodedata
from pathlib import Path

from PIL import Image

REPO = Path(__file__).resolve().parent.parent
OUT_TXT = REPO / "mica" / "poesie" / "it"
OUT_IMG = REPO / "static" / "img" / "poesie"
MAX_W = 1200
IMG_EXT = {".png", ".jpg", ".jpeg", ".webp", ".gif"}

LINK = re.compile(r"!?\[([^\]]*)\]\(\s*<?(?:\./)?([^)>]+?)>?\s*\)")


def slug(s):
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    return re.sub(r"[^a-z0-9]+", "-", s.lower()).strip("-")


def converti_immagine(src, dst):
    if dst.exists() and dst.stat().st_mtime >= src.stat().st_mtime:
        return False
    with Image.open(src) as im:
        im = im.convert("RGBA" if im.mode in ("RGBA", "LA", "P") else "RGB")
        if im.width > MAX_W:
            im = im.resize((MAX_W, round(im.height * MAX_W / im.width)), Image.Resampling.LANCZOS)
        im.save(dst, "WEBP", quality=80, method=6)
    return True


def importa(cartella, md, scritti, immagini):
    data = f"{cartella.name[:4]}-{cartella.name[4:6]}-{cartella.name[6:]}"
    righe = md.read_text(encoding="utf-8").splitlines()
    titolo = next((r[2:].strip() for r in righe if r.startswith("# ")), md.stem)

    def immagine(m):
        alt, target = m.group(1), m.group(2).strip()
        src = cartella / target
        if src.suffix.lower() not in IMG_EXT or not src.is_file():
            return m.group(0)  # link vero, non un'immagine locale
        dst = OUT_IMG / f"{data}-{slug(src.stem)}.webp"
        if converti_immagine(src, dst):
            immagini.append(dst.name)
        # alt vuoto = immagine decorativa: pandoc non aggiunge la didascalia
        return f"![{alt}](/static/img/poesie/{dst.name})"

    corpo = []
    for r in righe:
        r = LINK.sub(immagine, r)
        if r.startswith("    ") and r.strip():
            r = "> " + r.strip()
        corpo.append(r.rstrip())
    testo = (f"---\ntitle: {json.dumps(titolo, ensure_ascii=False)}\ncreated: {data}\n---\n\n"
             + "\n".join(corpo).strip() + "\n")

    dst = OUT_TXT / f"{data}-{slug(md.stem)}.md"
    if not dst.exists() or dst.read_text(encoding="utf-8") != testo:
        dst.write_text(testo, encoding="utf-8")
        scritti.append(dst.name)
    return dst.name


def main():
    sorgente = Path(sys.argv[1] if len(sys.argv) > 1 else "~/prj/poesie").expanduser()
    OUT_TXT.mkdir(parents=True, exist_ok=True)
    OUT_IMG.mkdir(parents=True, exist_ok=True)

    scritti, immagini, attesi = [], [], set()
    for cartella in sorted(p for p in sorgente.iterdir() if p.is_dir()):
        if not re.fullmatch(r"\d{8}", cartella.name):
            print(f"  ignorata (nome non AAAAMMGG): {cartella.name}")
            continue
        for md in sorted(cartella.glob("*.md")):
            attesi.add(importa(cartella, md, scritti, immagini))

    print(f"Testi aggiornati: {len(scritti)}")
    for x in scritti:
        print("  +", x)
    print(f"Immagini convertite: {len(immagini)}")
    orfani = sorted(p.name for p in OUT_TXT.glob("*.md") if p.name not in attesi)
    if orfani:
        print("Attenzione, nel sito ma non più nella sorgente (cancellali a mano se serve):")
        for x in orfani:
            print("  ?", x)


if __name__ == "__main__":
    main()
