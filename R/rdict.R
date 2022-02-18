#' An S4 class to represent a Dictionary data structure.
#'
#' @slot keys A character list of unique keys which are associated with values.
#' @slot values A list of values which are associated with unique keys.
#' @examples 
#' myDict = Dictionary()
#' 
#' myDict = Add(myDict, "key1", 1)
#' ContainsKey(myDict, "key1")
#' GetValue(myDict, "key1")
#' myDict = RemoveKey(myDict, "key1")
#' @exportClass Dictionary
#' @export Dictionary
Dictionary <- setClass(
  # Set the name for the class
  "Dictionary",
  
  # Define the slots
  slots = c(
    keys = "character",
    values = "list"
  ),
  
  # Set the default values for the slots.
  prototype=list(
    keys=character(),
    values=list()
  ),
  
  # Test that data structure is valid
  validity=function(object)
  {
    if(length(object@keys) != length(unique(object@keys))) {
      return("Keys must be unique.")
    }
    return(TRUE)
  }
)

# Create a function to check if key is in dictionary
#' A function to check if a key exists in the dictionary.
#'
#' @param object the rdict object
#' @param key the key
#' @examples 
#' myDict = rdict()
#' 
#' myDict = Add(myDict, "key1", 1)
#' ContainsKey(myDict, "key1")
#' @export
setGeneric(name="ContainsKey",
           def=function(object, key)
           {
             standardGeneric("ContainsKey")
           })

setMethod(f="ContainsKey",
          signature="Dictionary",
          definition=function(object, key)
          {
            return(key %in% object@keys)
          })

# Create a function to add key/value pair to dictionary
#' A function to add a key/value pair to the dictionary.
#'
#' @param object the rdict object
#' @param key the key
#' @param value the value
#' @examples 
#' myDict = rdict()
#' 
#' myDict = Add(myDict, "key1", 1)
#' @export
setGeneric(name="Add",
           def=function(object, key, value)
           {
             standardGeneric("Add")
           })

setMethod(f="Add",
          signature="Dictionary",
          definition=function(object, key, value)
          {
            if(key %in% object@keys) { stop(paste0("ERROR: key=", key, " already in dictionary, keys must be unique.")) }
            
            object@keys = c(object@keys, key)
            object@values[[key]] = value
            
            validObject(object)
            return(object)
          })

# Create a function to get a value using a key from dictionary
#' A function to get a value from the dictionary given a key.
#'
#' @param object the rdict object
#' @param key the key
#' @examples 
#' myDict = rdict()
#' 
#' myDict = Add(myDict, "key1", 1)
#' GetValue(myDict, "key1")
#' @export
setGeneric(name="GetValue",
           def=function(object, key)
           {
             standardGeneric("GetValue")
           })

setMethod(f="GetValue",
          signature="Dictionary",
          definition=function(object, key)
          {
            if(key %in% object@keys) {  
              return(object@values[[key]])
            }
            stop(paste0("ERROR: key=", key, " not in dictionary, use ContainsKey() to check before calling GetValue()."))
          })

# Create a function to remove key from dictionary
#' A function to remove a key from the dictionary.
#'
#' @param object the rdict object
#' @param key the key
#' @examples 
#' myDict = rdict()
#' 
#' myDict = Add(myDict, "key1", 1)
#' myDict = RemoveKey(myDict, "key1")
#' @export
setGeneric(name="RemoveKey",
           def=function(object, key)
           {
             standardGeneric("RemoveKey")
           })

setMethod(f="RemoveKey",
          signature="Dictionary",
          definition=function(object,key)
          {
            object@values = object@values[object@keys != key]
            object@keys = object@keys[object@keys != key]
            
            validObject(object)
            return(object)
          })

