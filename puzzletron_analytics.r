library(RMySQL)
library(ggplot2)
library(dplyr)
library(magrittr)
library(igraph)
library(reshape2)

pw <- readLines("pw.txt")

puzzletron <- src_mysql(dbname="puzzletron", host="sql-aws.randomfish.org",
                        user="puzzletron", password=pw)

testing_feedback <- as.data.frame(tbl(puzzletron, "testing_feedback")) %>%
  select(pid, fun, difficulty)

rounds <- as.data.frame(tbl(puzzletron, "rounds")) %>%
  select(rid, name)

answers <- as.data.frame(tbl(puzzletron, "answers")) %>%
  select(aid, pid)

answers_rounds <- as.data.frame(tbl(puzzletron, "answers_rounds"))

round.data <- left_join(answers, answers_rounds)
round.data <- left_join(round.data, rounds) %>%
  rename(round.name = name) %>%
  filter(round.name != "Events", !is.na(pid), pid != 433)

user_info <- as.data.frame(tbl(puzzletron, "user_info")) %>%
  select(uid, username)

authors <- as.data.frame(tbl(puzzletron, "authors"))
author.data <- left_join(authors, user_info)

editors <- as.data.frame(tbl(puzzletron, "approver_queue"))
editor.data <- left_join(editors, user_info)

testers <- as.data.frame(tbl(puzzletron, "doneTesting")) %>%
  select(uid, pid)
tester.data <- left_join(testers, user_info)

ffcers <- as.data.frame(tbl(puzzletron, "factcheck_queue"))
ffcer.data <- left_join(ffcers, user_info)

ts.data <- testing_feedback %>%
  filter(fun != 0, difficulty != 0) %>%
  group_by(pid) %>%
  summarise(fun.mean = sum(fun) / length(fun),
            fun.se = sqrt(var(fun) / length(fun)),
            difficulty.mean = sum(difficulty) / length(difficulty),
            difficulty.se = sqrt(var(difficulty) / length(difficulty)),
            n = length(fun))
ts.round.data <- left_join(ts.data, round.data) %>%
  filter(!is.na(aid))

fish <- ts.round.data %>%
  filter(rid==13) %>%
  arrange(desc(difficulty.mean))

model1 <- lm(difficulty.mean ~ fun.mean, data = ts.data)
model2 <- lm(fun.mean ~ difficulty.mean, data = ts.data)
model3 <- lm(fun.mean ~ n, data = ts.data)


quartz(width=8, height=6)
ggplot(ts.round.data, aes(x=fun.mean, y=difficulty.mean, colour=round.name)) + 
  geom_point(aes(x=fun.mean, y=difficulty.mean, size=n)) +
#  geom_errorbar(aes(ymin=difficulty.mean-difficulty.se,
#                     ymax=difficulty.mean+difficulty.se),
#                alpha=0.4) +
#  geom_errorbarh(aes(xmin=fun.mean-fun.se,
#                     xmax=fun.mean+fun.se),
#                 alpha=0.4) +
    theme_bw() + 
  scale_colour_discrete(name = "Round Name") +
  scale_size(name = "Number of Responses") +
  scale_x_continuous(name = "Mean Fun Score",
                     limits = c(1,5)) + 
  scale_y_continuous(name = "Mean Difficulty Score",
                     limits = c(1,5))


round.author.data <- left_join(round.data, author.data)
author.puzzles <- round.author.data %>%
  group_by(username) %>%
  summarise(num.puzzles = length(pid)) %>%
  arrange(desc(num.puzzles))
quartz(width=10, height=6)
ggplot(author.puzzles, aes(x=reorder(username, -num.puzzles,), y=num.puzzles)) + 
  geom_bar(stat="identity") + 
  theme_bw() + 
  xlab("") +
  ylab("Number of puzzles authored") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("~/Dropbox/Hunt/plots/authors.png", width=10, height=6)

round.editor.data <- left_join(round.data, editor.data)
editor.puzzles <- round.editor.data %>%
  group_by(username) %>%
  summarise(num.puzzles = length(pid)) %>%
  arrange(desc(num.puzzles))
quartz(width=6, height=6)
ggplot(editor.puzzles, aes(x=reorder(username, -num.puzzles,), y=num.puzzles)) + 
  geom_bar(stat="identity") + 
  theme_bw() + 
  xlab("") +
  ylab("Number of puzzles editored") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("~/Dropbox/Hunt/plots/editors.png", width=6, height=6)

round.tester.data <- left_join(round.data, tester.data)
tester.puzzles <- round.tester.data %>%
  group_by(username) %>%
  summarise(num.puzzles = length(pid)) %>%
  arrange(desc(num.puzzles))
quartz(width=13, height=6)
ggplot(tester.puzzles, aes(x=reorder(username, -num.puzzles,), y=num.puzzles)) + 
  geom_bar(stat="identity") + 
  theme_bw() + 
  xlab("") +
  ylab("Number of puzzles testsolved") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("~/Dropbox/Hunt/plots/testsolvers.png", width=13, height=6)

round.ffcer.data <- left_join(round.data, ffcer.data)
ffcer.puzzles <- round.ffcer.data %>%
  group_by(username) %>%
  summarise(num.puzzles = length(pid)) %>%
  arrange(desc(num.puzzles))
quartz(width=8, height=6)
ggplot(ffcer.puzzles, aes(x=reorder(username, -num.puzzles,), y=num.puzzles)) + 
  geom_bar(stat="identity") + 
  theme_bw() + 
  xlab("") +
  ylab("Number of puzzles factchecked") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("~/Dropbox/Hunt/plots/factcheckers.png", width=8, height=6)

test <- author.data %>%
  group_by(pid) %>%
  mutate()
author.puzz <- dcast(author.data, username ~ pid, length)
author.adj <- dcast(author.puzz, username ~ username, sum)

graph()