# Before starting

* Update R
* Update RStudio
* Update all R packages

# R
See the [Style Guide](https://gitlab.com/WEEL_grp/core/blob/master/Guides/R%20Style%20Guide/R_Style_Guide.pdf). 

Objects should have descriptive names, unless very generic (eg: DT, locs). 

> Anyone should be able to pick up where you left off.

This means we need to properly document our code, use the [Issue Board](https://gitlab.com/WEEL_grp/ewc/issues) to track progress, discuss decisions/challenges/successes!

# Git
See the [Getting Started with Git guide](https://gitlab.com/WEEL_grp/core/blob/master/Guides/Getting%20Started%20With%20Git/Getting_Started_With_Git.pdf).  

## Commits
Commits should be atomic:

* each commit should be one fix/task
* commit message should be brief, but describe this fix/task
* you should be mindful of what files you are committing (don't default to "stage all")

> If you can describe what you did in this commit in a short sentence and it makes sense, commit. [*](https://stackoverflow.com/questions/38155592/atomic-commits-best-practice)


In addition, be careful pushing large files/don't do it. 

## Pulling
Often, we can use `git rebase` instead of `git merge`, especially if you are pushing your changes and upstream changes are unrelated. See this [great stackoverflow answer](https://stackoverflow.com/a/804156/3481674) and this [tutorial](https://www.atlassian.com/git/tutorials/merging-vs-rebasing). 


# Issues
Use the [Issue Board](https://gitlab.com/WEEL_grp/ewc/issues) to discuss decisions, problems, develop methods. Learn more [here](https://docs.gitlab.com/ee/user/project/issue_board.html). 

<!-- labels, separate boards --> 

If you'd to like include a table, use `knitr::kable` and copy+paste the 
output. 

# Documentation
## CONTRIBUTING
(this document)


## README
Add output files (intermediate or final) to the table. 

##  TODO
Track TODOs, tasks, etc. 

Let's favor the [Issue Board](https://gitlab.com/WEEL_grp/ewc/issues).  

Similarly, let's try and put comments/notes that we'd otherwise put inside scripts on the Issue Board. 


## LICENSE
License used for this project. 


## Project structure 
See README.md and [Standard Project Structure](https://gitlab.com/WEEL_grp/core/tree/master/Guides/Standard%20Project%20Structure). 

Use subfolders in `/input`, `/output`, and `/R` to organize. 

No need to put dates in any file names, that's what Git is for. 