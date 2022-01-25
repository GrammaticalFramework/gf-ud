## Debugging feature:

Answer the question: Why doesn't this function apply?

Possible input formats:
* FunctionName 6 3 2
* FunctionName (Sub tree1) (More (Trees of stuff))
* FunctionName words for subtrees

What to show:
* When features doesn't match
* When labels and cats doesn't match


Maybe start from the final devtree and recreate work so we can prioritize simpler diffs.
We could even tell why a tree that was created wasn't included and
if a subtree of what we asked for is missing, we can answer that.

