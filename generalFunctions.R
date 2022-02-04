## general use functions
Function_diccionary = function(name = NULL){
  info = list(
    "rm_accent" = 
      "This function detects and remove special characters in a string.
  The arguments are the following:
  - str: Vector of strings that will be treated.
  - patterns: It's a vector with the strings that are going to be removed. 
  You can also input \'all\' to address all the special characters",
    
    "automateLineGGPLOT" = 
      "This function generates a simple ggplot code for a line plot.
  The arguments are the following:
  - daf: It's the dataframe that is going to be ploted.
  - x_axis_name: It's the name as a string of the variable in daf that is going 
  to be in the x axis of the plot.
  - variables: It's a vector with the name or indexes of daf's variables to be
  ploted.
  - palette: It's the palette of colors that are going to be used. It'll be
  necessary to have the package RColorBrewer.
  n_colors: The number of colors to be used. You can see the palette info using
  RColorBrewer::brewer.pal.info.
  - addToHead: It's an additional argument that will go inside ggplot(). You
  must begin with a coma, such as addToHead = \",group = 1\".",
    
    "find_FunctionsPkg" =
      "This function find the respective package for a given function name. 
      After using this function, only the basic package will be loaded.
    The arguments are the following:
    - f: The name of the function, entered as a string.
    - pkgs: It's a vector with the name of the packages you want to inspect. If
    It's null, dplyr will be used instead.",
    
    "open_packages" =
      "This function install only packages that are not installed. The argument
myPackages is a vector with the packages names."
  )
  if(is.null(name)) 
    return(names(info))
  else if(!(name %in% names(info)))
    message("This function doesn't exist in this script.")
  return(cat(info[[name]]))
}

rm_accent <- function(str,#this one function is not mine
                      pattern="all") {

    if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""),
                  paste(nudeSymbols, collapse=""), 
                  str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}

automateLineGGPLOT = function(daf, x_axis_name, variables, 
                              palette = NULL, n_colors = 8,
                              addToHead = ""){
  # browser()
  daf_name = deparse(substitute(daf))
  if(!(x_axis_name %in% colnames(daf)))
    stop("x_axis_name is not a variable in daf.")
  
  ggplot_head = paste0("ggplot(", daf_name,", aes(x = ", x_axis_name,
                       addToHead,")) ")
  variCheck = as.numeric(as.character(variables)) %>% suppressWarnings()
  
  if(any(!is.na(variCheck))){ ## if there are some numeric characters
    aa = !is.na(variCheck)
    variables[aa] = colnames(daf)[variables[aa]]
  }
  
  aa = which(variables %in% x_axis_name)
  if(length(aa) > 0){
    variables = variables[-aa]
  }
  
  if(is.null(palette)){
    plotColors = RColorBrewer::brewer.pal(n = 9, name = "Set1")
  }else{
    if(length(variables) > n_colors){
      message("The number os colors for the ploy is insuficcient.")
      message("pallete argument was changed to \"Set1\" ")
      palette = "Set1"
    }
    plotColors = RColorBrewer::brewer.pal(n = n_colors, name = palette)
    if(length(plotColors) < length(variables)){
      stop("There are too many variables")
    }
  }
  
  for (i in 1: length(variables)) {
    aux = paste0("+ geom_line(aes(y = ", variables[i], 
                 "), color = \"", plotColors[i],"\")")
    ggplot_head = paste0(ggplot_head, aux)
  }
  return(ggplot_head)
}

find_FunctionsPkg = function(f, pkgs = NULL){
  basic_pcks = c("stats", "graphics", "grDevices", "utils", 
                 "datasets", "methods", "base")
  message("Let's search the function f in the packages pkgs!")
  library(dplyr)
  if (is.null(pkgs)) 
    pkgs = "dplyr"
  pkgs_ = c(); j = 1
  for (i in 1:length(pkgs)) {
    invisible(lapply(pkgs[i], library, character.only = TRUE))
    # library(paste0("package:", pkgs[i]))
    functions = lsf.str(paste0("package:", pkgs[i])) %>% as.character()
    invisible(lapply(paste0("package:", pkgs[i]),
                     detach, character.only = TRUE))
    # detach(paste0("package:", pkgs[i]), unload=TRUE)
    if(f %in% functions){
      pkgs_[j] = pkgs[i]; j = j + 1
    }
  }
  current_pcks = .packages()
  current_pcks = current_pcks[!(current_pcks %in% basic_pcks)]
  if(length(current_pcks) > 1){
    for (i in 1:length(current_pcks)) { ## unload non basic packages
      invisible(lapply(paste0("package:", current_pcks[i]),
                       detach, character.only = TRUE))
    }
  }
  return(pkgs_)
}

open_packages <- function(myPackages = NULL){
  if(is.null(myPackages)) stop("Enter the packages you need")
  boolean_pkgs = myPackages %in% rownames(installed.packages())
  # pkgs.True = myPackages[boolean_pkgs]
  pkgs.False = myPackages[!boolean_pkgs]
  
  if(!(is.null(pkgs.False))) install.packages(pkgs.False)
  
  return(NULL)
}

daf_gathererMEC = function(daf, variables, except_var){
  if(!is.numeric(variables))
    stop("variables must be numerical representative")
  
  if(except_var %in% colnames(daf))
    aa = which(colnames(daf) %in% except_var)
  
  aa = which(variables %in% aa)
  if(length(aa) > 0)
    variables = variables[-aa]
  
  gathered = gather(data = daf,
                    key = "Setor",
                    value = "Value",
                    variables)
  gathered = gathered[,c(except_var, "Setor", "Value")]
  return(gathered)
}
