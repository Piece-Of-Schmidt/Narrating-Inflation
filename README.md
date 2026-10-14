# Narrating Inflation

Replication materials for the research paper **"Narrating Inflation: How German Economic Journalists Explain Post-Covid Price Rises"**.

The paper compares how German economic journalists and professional economists explain the 2022 inflation surge (survey, open-ended narratives) and how these explanations relate to newspaper coverage.

## Structure

| Folder | Content |
|---|---|
| `code/` | Analysis scripts, numbered in the order of the paper (`01`–`07`); `llm_inference/` holds the Anthropic API scripts for the media and validation coding |
| `data/survey/` | Survey responses of journalists and experts (`.csv` and `.RData`) |
| `data/narratives/` | Itemized narratives, second coding for intercoder reliability, spell-checked open answers, word-cloud helpers |
| `data/media/` | LLM labels per newspaper segment, human gold standard and LLM predictions for validation |
| `data/prompts/` | Final coding prompt (German) |
| `data/upi/` | Uncertainty-index counts (Fig. 1) |
| `output/` | Figures, tables and word-cloud terms as reported in the paper |
| `questionnaires/` | English questionnaires (experts, journalists) |

## Reproduction

Package requirements are listed in the `library()`/`import` calls at the top of each script.

The scripts in `code/llm_inference/` require an `ANTHROPIC_API_KEY` and the raw newspaper corpus (see below); the resulting labels are included in `data/media/`, so the analysis can be reproduced without re-running them.

## Data availability

The full-text newspaper articles are copyrighted and cannot be shared. Article-level LLM labels, the validation sample and the prompt are provided instead. Survey data are anonymized.

## License

[CC BY-NC-ND 4.0](LICENSE.md): attribution required, non-commercial use only, no modified versions of the data. For other arrangements, please get in touch.

## Contact

Tobias Schmidt  
TU Dortmund University  
tobias3.schmidt@tu-dortmund.de
