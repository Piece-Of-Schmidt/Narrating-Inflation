"""
LLM coding of the 2022 newspaper corpus (Section 3.2).

Sends every article segment to the Anthropic Message Batches API with the final prompt (data/prompts/prompt_media_coding_v5_de.txt, model claude-sonnet-3.5, temperature 0) and writes the parsed 0-3 labels per item.

Requires the restricted corpus file data/restricted/inflation_paragraphs_2022.csv (columns: id, text; see README) and the environment variable ANTHROPIC_API_KEY.

Usage (from the replication package root):
    python code/llm_inference/run_media_coding.py submit
    python code/llm_inference/run_media_coding.py fetch     # once the batch has ended

Output:
    output/llm/full_corpus_labels.csv   id + 10 labels (0-3); basis of data/media/media_llm_labels.csv
    output/llm/full_corpus_raw.jsonl    raw model responses
"""

import csv
import json
import os
import re
import sys

import anthropic
from anthropic.types.message_create_params import MessageCreateParamsNonStreaming
from anthropic.types.messages.batch_create_params import Request

MODEL = "claude-sonnet-3-5"
CORPUS = os.path.join("data", "restricted", "inflation_paragraphs_2022.csv")
OUT_DIR = os.path.join("output", "llm")
BATCH_ID_FILE = os.path.join(OUT_DIR, "batch_id.txt")
ID_MAP_FILE = os.path.join(OUT_DIR, "batch_id_map.csv")

PROMPT_KEYS = ["energy crisis", "expectations", "fiscal policy", "interest rates",
               "labour shortages", "money supply", "pent-up demand", "profiteering",
               "supply chains", "other"]

with open(os.path.join("data", "prompts", "prompt_media_coding_v5_de.txt"), encoding="utf-8") as f:
    PROMPT = f.read()


def load_corpus():
    with open(CORPUS, encoding="utf-8", newline="") as f:
        return [(row["id"], row["text"]) for row in csv.DictReader(f)]


def parse_labels(text):
    """Extract the final JSON label block from a model response."""
    m = re.findall(r"```json\s*(\{.*?\})\s*```", text, re.DOTALL) or re.findall(r"\{[^{}]*\}", text, re.DOTALL)
    if not m:
        return None
    try:
        obj = json.loads(m[-1])
    except json.JSONDecodeError:
        return None
    out = {}
    for k in PROMPT_KEYS:
        try:
            out[k] = max(0, min(3, int(obj.get(k, 0))))
        except (TypeError, ValueError):
            out[k] = 0
    return out


def submit():
    articles = load_corpus()
    os.makedirs(OUT_DIR, exist_ok=True)
    # custom_id must match ^[a-zA-Z0-9_-]{1,64}$
    id_map, requests = {}, []
    for i, (aid, text) in enumerate(articles):
        cid = aid if re.fullmatch(r"[a-zA-Z0-9_-]{1,64}", aid) else f"row{i:05d}"
        if cid in id_map:
            cid = f"{cid[:55]}_dup{i:04d}"
        id_map[cid] = aid
        requests.append(Request(custom_id=cid, params=MessageCreateParamsNonStreaming(
            model=MODEL, max_tokens=2048, temperature=0, system=PROMPT,
            messages=[{"role": "user", "content": text}])))

    with open(ID_MAP_FILE, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["custom_id", "article_id"])
        w.writerows(id_map.items())

    batch = anthropic.Anthropic().messages.batches.create(requests=requests)
    with open(BATCH_ID_FILE, "w", encoding="utf-8") as f:
        f.write(batch.id)
    print(f"Submitted {len(requests)} requests as batch {batch.id}")


def fetch():
    with open(BATCH_ID_FILE, encoding="utf-8") as f:
        batch_id = f.read().strip()
    with open(ID_MAP_FILE, encoding="utf-8", newline="") as f:
        id_map = {r["custom_id"]: r["article_id"] for r in csv.DictReader(f, delimiter=";")}

    client = anthropic.Anthropic()
    batch = client.messages.batches.retrieve(batch_id)
    print(f"Batch {batch_id}: {batch.processing_status} {batch.request_counts}")
    if batch.processing_status != "ended":
        sys.exit("Not finished yet - try again later.")

    rows, n_err, n_fail = {}, 0, 0
    with open(os.path.join(OUT_DIR, "full_corpus_raw.jsonl"), "w", encoding="utf-8") as raw:
        for result in client.messages.batches.results(batch_id):
            aid = id_map.get(result.custom_id, result.custom_id)
            if result.result.type != "succeeded":
                n_err += 1
                raw.write(json.dumps({"id": aid, "error": result.result.type}, ensure_ascii=False) + "\n")
                continue
            text = "".join(b.text for b in result.result.message.content if b.type == "text")
            raw.write(json.dumps({"id": aid, "response": text}, ensure_ascii=False) + "\n")
            labels = parse_labels(text)
            if labels is None:
                n_fail += 1
            else:
                rows[aid] = labels

    with open(os.path.join(OUT_DIR, "full_corpus_labels.csv"), "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["id"] + PROMPT_KEYS)
        for aid, _ in load_corpus():   # keep corpus order
            if aid in rows:
                w.writerow([aid] + [rows[aid][k] for k in PROMPT_KEYS])
    print(f"Parsed: {len(rows)} | API errors: {n_err} | parse failures: {n_fail}")


if __name__ == "__main__":
    {"submit": submit, "fetch": fetch}[sys.argv[1]]()
