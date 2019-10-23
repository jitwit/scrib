agent-iterations := 10000
agentA := Maggie
agentB := Monte

run : load.ss
	echo "(time (compare-agents $(agentA) $(agentB) $(agent-iterations))))" \
	| scheme -q --optimize-level 3 $<

Monte : load.ss
	echo "(run-cribbage-game Monte)" | scheme -q $<

clean :
	rm -rf *~
	rm -rf *html

.PHONY : clean run
