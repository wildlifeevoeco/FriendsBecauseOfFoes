# Git
## Commits
Commits should be atomic:

* each commit should be one fix/task
* commit message should be brief, but describe this fix/task
* you should be mindful of what files you are committing (don't default to "stage all")

Careful pushing large files/don't do it

## Pulling
Often, we can use `git rebase` instead of `git merge`, especially if you are pushing your changes and upstream changes are unrelated. See this [great stackoverflow answer](https://stackoverflow.com/a/804156/3481674) and this [tutorial](https://www.atlassian.com/git/tutorials/merging-vs-rebasing). 


# Issues
Use the [Issue Board](https://gitlab.com/WEEL_grp/ewc/issues) to discuss decisions, problems, develop methods. 

<!-- labels, separate boards --> 


# README
Add output files (intermediate or final) to the table. 

# Project structure 
See README.md and [Standard Project Structure](https://gitlab.com/WEEL_grp/core/tree/master/Guides/Standard%20Project%20Structure). 

Use subfolders in `/input`, `/output`, and `/R` to 


* subfolders in input, output and R 
* rebase
* update R and R packages, Rstudio
* see style guide (gitlab.com/WEEL_grp/core/R Style Guide)
* no need to put dates in file names, that's what version controlling is for
* objects have proper descriptive names (unless tabular DT, locs)
* anyone should be able to pick up after where you left off

