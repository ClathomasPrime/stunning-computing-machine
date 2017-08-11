
* Study notes:
	* workflow slide deck
	* mercurial 201


* `arc feature` between starting on anything
* bookmarks are EXACTLY nicknames for commit (hashes) - you don't even need them
* to update with a new master
	* `hg pull` ONLY updates master
	* `hg rebase` hefts your commit around - `hg rebase -b <commit> -d master<usually>`
	* both of above with `arc pull`
* conflict markers
	```
	<<<<< info
		stuff from master
	======
		stuff from my branch
	>>>>>> info

* Specifying commits - Revsets query language
  * (-r <revset>) in place of hash
  * <hash> = that commit
  * . = current commit
  * ^ = parent of
  * ~N = Nth ancestor
  * <rs>::<rs> = range in commit DAG 
    * (collection of all paths a -> b)
    * can leave off endpoints. Runs to root or heads
  * revset predicates
    * -r 'draft()'
      * eg: hg pull <...> ; hg rebase -d master -r 'draft()'
    * -r 'public()'
    * -r 'bookmarks()' : local bookmarks
      * those that bookmarks LITERALLY point to (not their ancestors or
        anything)
  * (set-like) operations
    * + = union
    * & = intersection
    * - = subtraction
    * e.g. 
  * revset fuctions
    * ancestor(r1, r2) : most recent common ancestor
    * -r 'r1 % r2' : ancestors of r1 which are not also ancestors of r2
      * -r '. % master' : path back to master
      * = (:: .) - (:: master)
      * = (:: .) & (master ::)
  * hg help revsets

* Undoing things
  * amend, rebase etc create NEW versions of commits
    * obsolete, old commits are hidden
    * commits have a (labeled graph-like) history of their own
  * revsets for obsolete commits
    * precursors(r) and successors(r) : ONE step (immediate neighbors) of r
    * allprecursors(r) and allsuccessors(r) : multistep
  * --hidden : add to any command and make everything visible
    * if not present, it looks to the command like hidden commits are totally not present
      at all
  * Basic amend undoing: look up (hidden) old version and switch to it and
    ignore the mistakeish version
    * `hg journal` to see hash of repo after each command run
      * `hg journal bookmark` to see old hashes of bookmark
    * `hg show --hidden` to see changes or something

  * explicit undos - no need to search for hashes
    * hg uncommit :
      * hides commit, updates to parent, places changes in working copy
    * hg unamend : same thing

  * discarding unwanted stuff
    * hg update --clean :
      * discard pending changes (hg add, editing tracked file, hg rm)
      * doesn't touch unknown files 
    * hg purge : delete unknown - dangerous

  * bad thing we should probably avoid:
    * revert : changes the working copy WITHOUT changing the working copy parent
      (ie "current commit") - that's weird
      * `hg revert -r HASH filename` might be pretty useful
    * `backout -r HASH` : creates inverse of specific commit
      * more precicely, updates working copy. Then YOU run commit
      * can have merge conflicts
      * use UNLAND button in phab
    * `reset HASH` : move working copy parent WITHOUT changing working copy

* Stacked commit workflows
  * How everybody except facebook usually works
  * moving around the stack: hg prev and hg next. 
    * --top / --bottom
    * --merge : shoves some changes with you as you move
    * --towards : branch some way
  * hg rebase : moves commits around
    * What to move: 
      * -r : some revision
      * -s (--source) : includes descendants
      * -b (--base) : entire subtree, ie (. % master)::
    * where to move it: -d (--dest)
    * keep old commits?: -k ??

  * amending in the middle of a stack "leaves children behind"
    * `hg rebase --restack` tries to fix it
      * `hg ammend --rebase`

  * `hg fold` commits together
    * only with linearly related, adjacent commits
  * `hg split` provides a nice UI for pulling commits apart
  * `hg commit -i` and `hg amend -i`
    * SELECT exactly the changes you want to commit/amend interactively
  * `hg strip` hides commits and all parents of commit
    * updating to a hidden commit automatically unhides it and its descendents
  * for-each-stacked : magic
    * for-each-stacked arc diff
    * run from top of stack
  * hg histedit : specify something to do at each commit in the stack
  * hg absorb
    * make all your changes at the top.
    * hg absorb trickles the changes down to the commit that changes those lines
    * `hg absorb -pn` to try it out - can't figure everything out always

* customization
  * scm-prompt : fburl.com/scmprompt

* infinitepush is a thing


