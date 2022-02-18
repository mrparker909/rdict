# rdict
R package implementing a Dictionary data structure.

# Install Instructions

```{r}
devtools::install_github("mrparker909/rdict")
```

# Example Use

```{r}
# create a dictionary called myDict
myDict = Dictionary()

# add the key/value pair (key1, 1) to myDict
myDict = Add(myDict, "key1", 1)

# check that key1 exists in myDict
ContainsKey(myDict, "key1")

# get the value stored at key1
GetValue(myDict, "key1")

# remove key1 from myDict
myDict = RemoveKey(myDict, "key1")
```
