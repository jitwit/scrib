agent-iterations := 10000
monte-iterations := 50

montemonte : load.ss
	echo "(time (parameterize ((monte-iterations $(monte-iterations))) (compare-agents Monte Monte $(agent-iterations))))" | scheme -q --optimize-level 3 $<

montemax : load.ss
	echo "(time (parameterize ((monte-iterations $(monte-iterations))) (compare-agents Monte Max $(agent-iterations))))" | scheme -q --optimize-level 3 $<

montemindi : load.ss
	echo "(time (parameterize ((monte-iterations $(monte-iterations))) (compare-agents Mindi Monte $(agent-iterations))))" | scheme -q --optimize-level 3 $<

mindimax : load.ss
	echo "(time (compare-agents Mindi Max $(agent-iterations))))" | scheme -q --optimize-level 3 $<

clean :
	rm -rf *~
	rm -rf *html

.PHONY : clean montemonte montemax mindimax
