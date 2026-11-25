#!/usr/bin/env python3
"""Aggiorna static/plants.json (schede del prontuario) dal registro delle preparazioni.

Il registro (es. ~/prj/spagyria/plants.json -> spagyria.json) contiene un record per
ogni preparazione: seq, nome, nomeef, pianeti, segni, descrizione, ricetta, tempi.
static/plants.json contiene invece una scheda per pianta, con l'elenco delle sue
preparazioni in "preparazioni".

Lo script:
  - aggiunge alla scheda giusta le preparazioni del registro non ancora presenti
    (abbinamento per nome/nomeef della scheda o per un nome già usato in una sua
    preparazione, senza distinzione maiuscole/minuscole);
  - se la pianta non ha ancora una scheda, ne crea una di base a partire dal
    record (pianeti, segni, indicazioni in MAIUSCOLO, descrizione) da rifinire a mano;
  - non pubblica mai "ricetta" e "tempi";
  - non modifica le schede esistenti: testi, avvertenze, ecc. si curano a mano
    direttamente in static/plants.json.

Uso:
  python3 scripts/aggiorna_piante.py ~/prj/spagyria/plants.json            # anteprima
  python3 scripts/aggiorna_piante.py ~/prj/spagyria/plants.json --scrivi   # salva

Per aggiungere una preparazione di una pianta già presente ma registrata con un
nome diverso (es. "Cardo Mariano Sylibum Marianum"), aggiungila a mano in
"preparazioni" della scheda: da lì in poi quel nome verrà riconosciuto.
"""
import argparse
import json
import re
from pathlib import Path

SITO = Path(__file__).resolve().parent.parent / "static" / "plants.json"

PIANETI = ["Sole", "Luna", "Mercurio", "Venere", "Marte", "Giove", "Saturno"]
SEGNI = ["Ariete", "Toro", "Gemelli", "Cancro", "Leone", "Vergine", "Bilancia",
         "Scorpione", "Sagittario", "Capricorno", "Acquario", "Pesci"]
ORDINE = ["nome", "nomeef", "preparazioni", "pianeti", "segni", "note_astrologiche",
          "indicazioni", "descrizione", "psicosomatica", "uso_esterno", "parti_utilizzate",
          "osservazioni", "avvertenze", "note", "fonti"]


def chiave(s):
    return re.sub(r"\s+", " ", (s or "")).strip().lower()


def ordine_seq(seq):
    """Ordine naturale: 5 < 5b < 6 < F7 < 8."""
    m = re.match(r"^F?(\d+)(.*)$", str(seq))
    return (int(m.group(1)), m.group(2)) if m else (10**9, str(seq))


def lista(raw, canonici):
    out = []
    for tok in re.split(r"[,:|\s\-]+", raw or ""):
        tok = tok.strip().capitalize()
        if tok == "12" and canonici is SEGNI:
            tok = "Tutti i segni"
        if (tok in canonici or tok == "Tutti i segni") and tok not in out:
            out.append(tok)
    return out


def scheda_da_record(r):
    """Scheda di base per una pianta nuova."""
    righe = [l.strip() for l in (r.get("descrizione") or "").split("\n")]
    while righe and not righe[0]:
        righe.pop(0)
    indicazioni = []
    while righe and righe[0] and sum(c.isupper() for c in righe[0]) > 0.8 * sum(c.isalpha() for c in righe[0]):
        voce = righe.pop(0).lower()
        voce = voce[:1].upper() + voce[1:]
        if voce not in indicazioni:
            indicazioni.append(voce)
    paragrafi = re.split(r"\n{2,}", "\n".join(righe))
    descrizione = "\n\n".join(p.strip() for p in paragrafi if p.strip())
    scheda = {
        "nome": r["nome"].strip(),
        "preparazioni": [],
        "pianeti": lista(" ".join(filter(None, [r.get("pianeti"), r.get("pianteti")])), PIANETI),
        "segni": lista(r.get("segni"), SEGNI),
        "indicazioni": indicazioni,
        "descrizione": descrizione,
    }
    nomeef = (r.get("nomeef") or "").strip()
    if nomeef and chiave(nomeef) != chiave(scheda["nome"]):
        scheda["nomeef"] = nomeef
    return scheda


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("registro", help="JSON delle preparazioni (spagyria.json)")
    ap.add_argument("--sito", default=str(SITO), help="plants.json del sito (default: %(default)s)")
    ap.add_argument("--scrivi", action="store_true", help="salva le modifiche (altrimenti solo anteprima)")
    a = ap.parse_args()

    registro = json.loads(Path(a.registro).read_text(encoding="utf-8"))
    schede = json.loads(Path(a.sito).read_text(encoding="utf-8"))

    per_nome, presenti = {}, set()
    for s in schede:
        s.setdefault("preparazioni", [])
        nomi = [s["nome"], s.get("nomeef")] + [p.get("nome") for p in s["preparazioni"]]
        for n in filter(None, nomi):
            per_nome.setdefault(chiave(n), s)
        for p in s["preparazioni"]:
            presenti.add((str(p["seq"]), chiave(p.get("nome") or s["nome"])))

    aggiunte, nuove = [], []
    for r in registro:
        seq, nome = str(r["seq"]), r["nome"].strip()
        if (seq, chiave(nome)) in presenti:
            continue
        s = per_nome.get(chiave(nome)) or per_nome.get(chiave(r.get("nomeef")))
        if s is None:
            s = scheda_da_record(r)
            schede.append(s)
            per_nome[chiave(nome)] = s
            nuove.append(nome)
        s["preparazioni"].append({"seq": seq, "nome": nome})
        s["preparazioni"].sort(key=lambda p: ordine_seq(p["seq"]))
        presenti.add((seq, chiave(nome)))
        aggiunte.append(f"N° {seq} {nome} -> {s['nome']}")

    nel_registro = {(str(r["seq"]), chiave(r["nome"])) for r in registro}
    orfane = [f"N° {p['seq']} {p.get('nome')} (scheda {s['nome']})"
              for s in schede for p in s["preparazioni"]
              if (str(p["seq"]), chiave(p.get("nome") or s["nome"])) not in nel_registro]

    print(f"Preparazioni aggiunte: {len(aggiunte)}")
    for x in aggiunte:
        print("  +", x)
    if nuove:
        print("Schede nuove (da rifinire a mano: avvertenze, uso esterno, ecc.):")
        for x in nuove:
            print("  *", x)
    if orfane:
        print("Attenzione, preparazioni non trovate nel registro:")
        for x in orfane:
            print("  ?", x)

    if a.scrivi and aggiunte:
        out = [{k: s[k] for k in ORDINE if k in s} for s in schede]
        Path(a.sito).write_text(json.dumps(out, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
        print(f"Salvato {a.sito}")
    elif aggiunte:
        print("(anteprima: usa --scrivi per salvare)")


if __name__ == "__main__":
    main()
