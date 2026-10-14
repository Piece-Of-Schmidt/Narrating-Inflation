# -*- coding: utf-8 -*-
"""
Figure C (step 2/2): translate the German top terms (output of 06_wordcloud_terms.R)
into English and draw one word cloud per group. Terms occurring in both clouds are
dark green, group-specific terms blue.

The German-English translation table is shipped in data/narratives/wc_translations.csv.
To regenerate it with gpt-4o-mini (requires OPENAI_API_KEY):
    python code/07_wordclouds.py --translate

Usage (from the replication package root):
    python code/07_wordclouds.py
"""

import json
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd
from wordcloud import WordCloud

TERMS = "output/wordcloud/wc_terms_de.csv"
TRANSLATIONS = "data/narratives/wc_translations.csv"
OUT = "output/figures/figC_wordclouds.png"

PROMPT = (
    "The following German words are the most frequent terms in survey answers in which "
    "economists and journalists explain the causes of inflation in Germany in 2022. "
    "Translate each word into English as used in that context. Use lowercase. If the "
    "translation needs several words, join them with underscores (e.g. 'lieferketten' -> "
    "'supply_chains'). Return a JSON object mapping every German word to its translation.\n\n"
)


def translate(words):
    from openai import OpenAI
    resp = OpenAI().chat.completions.create(
        model="gpt-4o-mini",
        temperature=0,
        response_format={"type": "json_object"},
        messages=[{"role": "user", "content": PROMPT + "\n".join(words)}],
    )
    mapping = json.loads(resp.choices[0].message.content)
    missing = [w for w in words if w not in mapping]
    if missing:
        sys.exit(f"Translation missing for: {missing}")
    return mapping


def main():
    terms = pd.read_csv(TERMS, encoding="utf-8")

    if "--translate" in sys.argv:
        # order German words by overall weight so that the most frequent word keeps the plain label
        order = terms.groupby("term")["val"].sum().sort_values(ascending=False).index.tolist()
        mapping = translate(order)
        table = pd.DataFrame({"de": order, "en": [mapping[w].strip().lower() for w in order]})
        # distinct German words with the same translation get a distinguishing number
        table["en"] = table["en"] + table.groupby("en").cumcount().add(1).map(lambda i: "" if i == 1 else str(i))
        table.to_csv(TRANSLATIONS, index=False, encoding="utf-8")

    table = pd.read_csv(TRANSLATIONS, encoding="utf-8")
    terms["en"] = terms["term"].map(dict(zip(table["de"], table["en"])))
    if terms["en"].isna().any():
        sys.exit("Terms without translation - rerun with --translate.")

    freqs = {g: dict(zip(d["en"], d["val"])) for g, d in terms.groupby("group")}
    shared = set(freqs["experts"]) & set(freqs["journalists"])

    def color(word, **kwargs):
        return "green" if word in shared else "blue"

    fig, axs = plt.subplots(1, 2, figsize=(12, 6))
    for ax, group, title in zip(axs, ["experts", "journalists"], ["Experts", "Journalists"]):
        wc = WordCloud(width=400, height=250, background_color="white", random_state=1)
        wc.generate_from_frequencies(freqs[group]).recolor(color_func=color)
        ax.imshow(wc, interpolation="bilinear")
        ax.set_title(title)
        ax.axis("off")
    plt.savefig(OUT, dpi=300, bbox_inches="tight")
    print(f"Saved {OUT} ({len(shared)} shared terms)")


if __name__ == "__main__":
    main()
