
test:
	# chibi-scheme -A. trie-test-chibi.scm
	# chibi-scheme -A. hierarchy-test-chibi.scm
	# chibi-scheme -A. methods-test.scm
	rm -f *.log
	gosh -r7 -A. tests.scm
	

tags:
	ctags -R .
