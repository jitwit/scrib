iterations := 10000
agentA := Maggie
agentB := Monte
daily-hands := 5

run : load.ss
	echo "(time (compare-agents $(agentA) $(agentB) $(iterations))))" | \
		scheme -q --optimize-level 3 $<

frequency-tables : load.ss
	echo "(build-all-tables)" | scheme -q --optimize-level 3 $<

daily-hand : daily-cribbage-hand.ss
	echo "(parameterize ((display-length $(daily-hands))) (main))" | scheme -q --optimize-level 3 $<

clean :
	rm -rf *~
	rm -rf *html

.PHONY : clean run frequency-tables
