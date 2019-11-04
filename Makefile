iterations := 10000
agentA := Maggie
agentB := Monte
daily-hands := 5
growth-factor := 1000

run : load.ss
	echo "(time (compare-agents $(agentA) $(agentB) $(iterations))))" | \
		scheme -q --optimize-level 3 $<

grow : load.ss
	echo "(grow-win-table Max Max 0 $(growth-factor))" | scheme -q $<

frequency-tables : load.ss
	echo "(build-all-tables)" | scheme -q --optimize-level 3 $<

daily-hand : daily-cribbage-hand.ss
	echo "(parameterize ((display-length $(daily-hands))) (main))" | scheme -q --optimize-level 3 $<

win-chart : calculations/win-probability-table
	echo "(render-table \"$<\")" | scheme load.ss

clean :
	rm -rf *~
	rm -rf *html

.PHONY : clean run frequency-tables grow
