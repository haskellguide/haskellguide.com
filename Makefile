install: 
	git add -A
	git commit -m bump
	git pull origin --rebase
	git push origin master
