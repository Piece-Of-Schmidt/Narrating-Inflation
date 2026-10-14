# -*- coding: utf-8 -*-
"""
LLM coding of the 151 human-coded validation segments (Section 3.2, Table 2).

Same model, prompt and settings as run_media_coding.py, sent as individual requests. The predictions are evaluated against the human gold standard in code/05_media_analysis.R.

Requires data/restricted/inflation_paragraphs_2022.csv and the environment variable ANTHROPIC_API_KEY.

Usage (from the replication package root):
    python code/llm_inference/run_validation_coding.py

Output:
    output/llm/validation_llm_predictions.csv   basis of data/media/validation_llm_predictions.csv
"""

import csv
import os
import time
from concurrent.futures import ThreadPoolExecutor

import anthropic

from run_media_coding import MODEL, PROMPT, PROMPT_KEYS, CORPUS, OUT_DIR, parse_labels

GOLD = os.path.join("data", "media", "validation_gold_standard.csv")

client = anthropic.Anthropic()


def call_api(text, retries=3):
    for attempt in range(retries):
        try:
            msg = client.messages.create(model=MODEL, max_tokens=2048, temperature=0,
                                         system=PROMPT, messages=[{"role": "user", "content": text}])
            return msg.content[0].text
        except anthropic.APIError as e:
            print(f"API error (attempt {attempt + 1}): {e}")
            time.sleep(10 * (attempt + 1))
    return None


def main():
    with open(CORPUS, encoding="utf-8", newline="") as f:
        text_by_id = {r["id"]: r["text"] for r in csv.DictReader(f)}
    with open(GOLD, encoding="utf-8", newline="") as f:
        ids = [r["id"] for r in csv.DictReader(f)]

    def code(aid):
        resp = call_api(text_by_id[aid])
        return parse_labels(resp) if resp else None

    with ThreadPoolExecutor(max_workers=10) as pool:
        preds = list(pool.map(code, ids))

    os.makedirs(OUT_DIR, exist_ok=True)
    with open(os.path.join(OUT_DIR, "validation_llm_predictions.csv"), "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(["id"] + [k.replace(" ", ".").replace("-", ".") for k in PROMPT_KEYS])
        for aid, p in zip(ids, preds):
            w.writerow([aid] + ([p[k] for k in PROMPT_KEYS] if p else ["NA"] * len(PROMPT_KEYS)))
    print(f"Coded {sum(p is not None for p in preds)} / {len(ids)} validation segments.")


if __name__ == "__main__":
    main()
