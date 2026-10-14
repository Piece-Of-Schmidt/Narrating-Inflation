library(quanteda)

nar = read.csv("data/narratives/narratives_spellchecked.csv", encoding = "UTF-8")   # spell-checked open answers
stops = readLines("data/narratives/stopwords_de.txt", encoding = "UTF-8")

top_terms = function(txt, group) {
  txt = sub("elpolitik", "eldpolitik", txt)   # fix recurring typo "Gelpolitik"
  toks = tokens(corpus(txt), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE,
                remove_url = TRUE, split_hyphens = TRUE)
  toks = tokens_remove(toks, c(stops, "inflation", "preise",
                               "zudem", "hinzu", "infolge", "zuge", "führt", "aufgrund", "faktor"))
  tf = topfeatures(dfm(toks), 100)
  data.frame(term = names(tf), val = tf / max(tf), group = group, row.names = NULL)
}

terms = rbind(top_terms(nar$narrative[nar$group == "experts"], "experts"),
              top_terms(nar$narrative[nar$group == "journalists"], "journalists"))

# save locally
# write.csv(terms, "output/wordcloud/wc_terms_de.csv", row.names = FALSE, fileEncoding = "UTF-8")
