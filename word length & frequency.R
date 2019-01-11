#Loading required Packages
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
#Loading Chapter 02 File
f_pre <- readLines("Chapter 02.txt")

#Converting the file content into string.
str(f_pre)
f_pre[1:10]

#1. Function to convert the vectors of lines to a vectors where each element is a word
#There are two ways to separate the words one is using regex and other one is str_split().
#I am using the str_split(). We can also use regex to separate the words and its syntax will be like 
#str_extract_all(f_pre, "[\\w|[',?.-]]+") or str_extract_all(f_pre[1], "[^\\s]+") both regex gives the same required characters.
convert_to_words_vector <- function(f_pre)
{
  separate_words <- unlist(str_split(f_pre, " "))
  #removing the empty string from the characters and returing the required result
  return( separate_words[separate_words!= ""]) 
}

f_pre_vec <- convert_to_words_vector(f_pre)

str(f_pre_vec)
f_pre_vec[1:10]

#2. Function to pre-process the data, remove invalid characters, empty string, lowercase words and remove stopwords.
pre_processed_data <- function(value){
  #stopwords package(if needed).
  stop_words <- c("a", "about", "above", "above", "across", "after", "afterwards", "again", 
                 "against", "all", "almost", "alone", "along", "already", "also","although",
                 "always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", 
                 "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  
                 "at", "back","be","became", "because","become","becomes", "becoming", "been", 
                 "before", "beforehand", "behind", "being", "below", "beside", "besides", 
                 "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", 
                 "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", 
                 "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", 
                 "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", 
                 "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", 
                 "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", 
                 "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", 
                 "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", 
                 "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", 
                 "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", 
                 "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", 
                 "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", 
                 "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", 
                 "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", 
                 "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", 
                 "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", 
                 "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", 
                 "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", 
                 "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", 
                 "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system",
                 "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", 
                 "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", 
                 "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", 
                 "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward",
                 "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", 
                 "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", 
                 "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", 
                 "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", 
                 "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", 
                 "your", "yours", "yourself", "yourselves", "the")
  
  #Converting stop_words into regex query form. I am first collapsing each stop_word with ^$ so that the whole words is checked and for 
  #the first and last word adding ^ and $ as they are lacking with these symbols while creating the regex query.
  regex_stopwords <- str_c("(^", str_c(stop_words, collapse = "$|^") ,"$)") 
  
  #Preparing invalid_characters to use it as a regex query using str_c, alternatively we can use other functions like paste (if needed)
  invalid_characters <- str_c(c("--","\\?","\\!","\\.",",","\\.","'",":"), collapse = "|") 
  
  #Replacing invalid charaters with empty string
  f_after_ic <- str_replace_all(value, invalid_characters, "") 
  
  #Converting the charters into lower cases using stringr build in function str_to_lower().
  f_after_lower <- str_to_lower(f_after_ic) 
  
  #Removing the stop words using regex. There might be other ways like using %in% etc.
  f_after_stopwords <- str_replace_all(f_after_lower, regex_stopwords, "")
  
  #Removing the empty string at the end to minimise the line of codes and returing the character.
  return(f_after_stopwords[f_after_stopwords!= ""])
}

f_post <- pre_processed_data(f_pre_vec)
str(f_post)

#4. Creating a tibble of 3 column. One for words, seccond is for pattern and third column is for word_length
#We can use unique() and other alternative is using the distinct() to remove the duplicacy of words. Distinct syntax will be like
#distinct(tibble(Words= f_post, Pattern= str_c("^", f_post, "$"), WLength = str_length(f_post)))
#for Wlength we can also use str_count().
ans <- tibble(Words= f_post, Pattern= str_c("^", f_post, "$"), WLength = str_length(f_post)) %>% unique()
ans

#5.tibble	 that	 contains	the frequency	 of	word	length	occurrence for	 the	text.
#Using the summarise and count (n()) to perform this task.
freq <- ans %>% group_by(WLength) %>% summarise(WFrequency = n())
freq

#6.Plot	the	results	for	the	chapter	text	analysis.
#Using the labs() for labelling the x and y axis.
ggplot(freq, mapping = aes(WLength, WFrequency)) + geom_line() + geom_point(color="blue") + labs(x= "Word Length", y="Word Frequency")
