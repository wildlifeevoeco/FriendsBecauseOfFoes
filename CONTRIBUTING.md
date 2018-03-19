# Git
## Commits
Commits should be atomic:

* each commit should be one fix/task
* commit message should be brief, but describe this fix/task
* you should be mindful of what files you are committing (don't default to "stage all")

Careful pushing large files/don't do it

## Pulling
Often, we can use `git rebase` instead of `git merge`, especially if you are pushing your changes and upstream changes are unrelated. See [this great stackoverflow answer](https://stackoverflow.com/a/804156/3481674).

[<img src="https://wac-cdn.atlassian.com/dam/jcr:e229fef6-2c2f-4a4f-b270-e1e1baa94055/02.svg?cdnVersion=ka" style="width: 30%; height: 30%">](merge)[<img src="https://wac-cdn.atlassian.com/dam/jcr:5b153a22-38be-40d0-aec8-5f2fffc771e5/03.svg?cdnVersion=ka" style="width: 30%; height: 30%">](merge)



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

