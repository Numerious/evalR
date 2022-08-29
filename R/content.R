# logic borrowed from the tufte package
#' @noRd
quote_footer <- function (text) 
{
    if (knitr::is_html_output()) {
        sprintf("<footer>%s</footer>", text)
    }
    else if (knitr::is_latex_output()) {
        sprintf("\\hfill %s", text)
    }
    else {
        warning("quote_footer() only works for HTML and LaTeX output", 
            call. = FALSE)
        text
    }
}


# two parameter operators
# The order determines the precedence
# These are default values across three functions. Hence, they are defined here to keep the code dry.
#' @noRd

default_binary_operators <- c(",", "|", "&", "<=", "<", ">=", ">", "==", "!=", "+", "-", "*", "%/%", "/", "%%", "%in%", ":", "^")

# 1 parameter operators
# The order determines the precedence
# These are default values across three functions. Hence, they are defined here to keep the code dry.
#' @noRd

default_singular_operators <- c("-", "!")

# default functions to check for
# These are default values across three functions. Hence, they are defined here to keep the code dry.
#' @noRd

default_valid_functions <- c("log", "c", "any", "all", "abs", "ifelse")




#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param text the string/code/statement you want to parse.
#'
#'
#' @name text_ref

NULL



#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param singular_operators tokens of length 1 that operate on a right hand value. For example, the `-` token is an operator to negate a vector. \code{NULL} value will be replaced with \code{c("-", "!")}.
#'
#'
#' @name singular_operators_ref

NULL




#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param binary_operators tokens of any length that operate on a left and right hand values. For example, the `+` token is an operator that adds a left vector to a right vector. \code{NULL} value will be replaced with \code{c(",", "|", "&", "<=", "<", ">=", ">", "==", "!=", "+", "-", "*", "\%/\%", "/", "\%\%", "\%in\%", ":", "^")}. The order determines the precedence of the operators.
#'
#'
#' @name binary_operators_ref

NULL





#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param valid_functions tokens of any length that are prefixed on a parenthesis block and specify a function to run on the provided parameters within the block. For example, the `log` token will evaluate the logarithm value of the first parameter. Note named parameters are not support. \code{NULL} value will be replaced with \code{c("log", "c", "any", "all", "abs", "ifelse")}.
#'
#'
#' @name valid_functions_ref

NULL




#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param map a named list of data.frames/lists/matrices. Where names are keys for referencing the values in the \code{text} parameters.
#'
#'
#' @name map_ref

NULL




#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param mapping_names optional argument to make the function faster or limit which map elements can be referenced.
#'
#'
#' @name mapping_names_ref

NULL





#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param tree the output object from \link{create_tree}
#'
#'
#' @name tree_ref

NULL



#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param pval the pval branch of a \code{tree}
#'
#'
#' @name pval_ref

NULL


#' Helper to find first block of parenthesis
#'
#' @description
#' This function will search for the first block of parenthesis and return it if found. Otherwise, it will return "".
#'
#' @inheritParams text_ref
#'
#' @return a substring. Either "" or the first parenthesis block.
#'
#' @export
#'
#' @examples
#' # returns ""
#' find_parenthesis("3 + 5")
#' # returns "(3 + 5)"
#' find_parenthesis("2 * (3 + 5)")
find_parenthesis <- function(text) {
  p_start <- -1
  p_end <- -1
  ps_count <- 0
  p_count <- 0
  found <- grepl("(", text, fixed = TRUE)
  if (found) {

    # the idea is to loop of each char and count how many parenthesis are encountered.
    # Once the closing parenthesis of the first opening parenthesis has been found, then break out of the loop.

    for (i in seq_len(nchar(text))) { # walk over each char

      x <- substr(text, start = i, stop = i)
      if (x == "(") { # check if element is an opening parenthesis

        ps_count <- ps_count + 1
        p_count <- p_count + 1
      } else if (x == ")") { # check if element is an closing parenthesis

        p_count <- p_count - 1
      }

      if (ps_count == 1 && p_start == -1) { # check if this is the first time we encountered an opening parenthesis

        p_start <- i
      } else if (ps_count > 0 && p_count == 0 && p_end == -1) { # check if we found the closing parenthesis of the first opening parenthesis

        p_end <- i
        break
      }
    }

    if ((p_start >= 0) & (p_end >= 0)) # make sure we found a opening parenthesis and closing
      {
        return(substr(text, start = p_start, stop = p_end))
      } else { # no opening and closing parenthesis found.

      return("")
    }
  } else { # no parenthesis is found. Hence, return ""
    return("")
  }
}
#' Convert a statement into an evaluation tree
#'
#' @description
#' function will break \code{text} into a list of lists.
#'
#' @details
#' See \code{vignette("Overview", package = "evalR")}
#'
#' @inheritParams text_ref
#' @inheritParams singular_operators_ref
#' @inheritParams binary_operators_ref
#' @inheritParams valid_functions_ref
#'
#' @return a list of lists. In other words, a tree data structure made from lists.
#'
#' @export
#'
#' @examples
#' x <- create_tree("2 * (3 + 5)")
#' str(x)
create_tree <- function(text, singular_operators = NULL, binary_operators = NULL, valid_functions = NULL) {

  # set default singular operators
  if (is.null(singular_operators)) {
    singular_operators <- default_singular_operators
  }

  # set default binary_operators
  if (is.null(binary_operators)) {
    binary_operators <- default_binary_operators
  }

  # set default valid_functions
  if (is.null(valid_functions)) {
    valid_functions <- default_valid_functions
  }



  # each tree is made up of two branches.
  # `pval` - stands for parenthesis values
  # `eval` - stands for evaluation values

  # start - create the pval branch

  pval_branch <- list()

  # check to see if any parenthesis block can be found.
  parenthesis_block <- find_parenthesis(text)
  while (parenthesis_block != ""){ # keep looping until no new block can be found.
  
    # item_i will be a unique index for this element
    item_i <- length(pval_branch)

    # pval_name is the name for this pval element and what will be used to replace the parenthesis block in the text.
    pval_name <- paste0("\\", item_i)

    # remove parenthesis and trim the inner substring.

    trim_parenthesis_block <- stringr::str_trim(substr(parenthesis_block, start = 2, stop = nchar(parenthesis_block) - 1), side = "both")

    # treat the inner substring like a new statement that needs to be converted into a tree.
    pval_branch[[pval_name]] <- create_tree(text = trim_parenthesis_block, singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions)

    # replace the parenthesis block with the pval element name
    text <- gsub(parenthesis_block, pval_name, text, fixed = T)


    # check again to see if another parenthesis block can be found.
    parenthesis_block <- find_parenthesis(text)
  }

  # end - create the pval branch

  # start - create the eval branch

  # all parenthesis blocks have been replace. Now create the remaining statement into a tree
  eval_branch <- create_eval_tree(text = text, singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions)

  # end - create the eval branch

  # create the tree structure

  tree <- list("pval" = pval_branch, "eval" = eval_branch)
  return(tree)
}




#' Convert a statement into an evaluation tree
#'
#' @description inner work horse for create_tree
#'
#' @inheritParams text_ref
#' @inheritParams singular_operators_ref
#' @inheritParams binary_operators_ref
#' @inheritParams valid_functions_ref
#'
#' @return a list
#'
#' @noRd
create_eval_tree <- function(text, singular_operators = NULL, binary_operators = NULL, valid_functions = NULL) {

  text <- stringr::str_trim(text, side = "both")
  nstr <- nchar(text)
  if (nstr == 0) { # no text string should be length 0
    stop("'text' is of length 0.")
  } else if (nstr == 1) { # by definition, a string of length 1 must be an atomic element
    return(list("atomic", text))
  }


  if (nstr > 2) { # for a binary operators to be include, the string must be at least 3 characters long

    minus_first <- substr(text, 2 , nchar(text))
    for (binary_operator_i in binary_operators) {
      found <- grepl(binary_operator_i, minus_first, fixed = TRUE)
      if (found) {
        # add one because we are starting a way from the first char
        start_i <- gregexpr(binary_operator_i, minus_first, fixed = T)[[1]][1] + 1


        # This node of the tree will have 3 elements
        # 1 - the binary operator
        # 2 - left side value
        # 3 - right side value
        start_string <- stringr::str_trim(substr(text, 1, start_i - 1), side = "both")
        end_string <- stringr::str_trim(substr(text, start_i + nchar(binary_operator_i) , nchar(text)), side = "both")

        return_list <- list(
          binary_operator_i,
          create_eval_tree(start_string, singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions),
          create_eval_tree(end_string, singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions)
        )
        return(return_list)
      }
    }
  } # if (nstr > 2)




  start_operator <- substr(text, 1, 1)
  if (start_operator %in% singular_operators) {
    end_string <- substr(text, 2, nchar(text))

    # it's possible the end_string could be a function call
    return_list <- list(
      start_operator,
      create_eval_tree(end_string, singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions)
    )
    return(return_list)
  }


  if (nstr > 2) {
    for (valid_functions_i in valid_functions) {
      found <- grepl(paste0(valid_functions_i, "\\"), text, fixed = TRUE)
      if (found) {
        found <- grepl(paste0("^", valid_functions_i, "\\\\[0-9]+$"), text, fixed = F)
        if (found) {
          # This node of the tree will have 2 elements
          # 1 - the valid function text
          # 2 - the parenthesis block lookup token


          end_string <- stringr::str_trim(substr(text, nchar(valid_functions_i) + 1, nchar(text)), side = "both")
          # the look up is assumed to be the only thing.
          return_list <- list(valid_functions_i,  list("atomic", end_string))
          return(return_list)
        }
      }
    }
  } # if (nstr > 2)


  # if the text doesn't match any other pattern, then return it as an atomic node.
  # in the eval functions, this will be cast to either a logical or numeric value.
  return(list("atomic", text))
}




#' safely evaluate text
#'
#' @description
#' Safe alternative to using eval + parse
#'
#' @details 
#' See \code{vignette("Overview", package = "evalR")}
#'
#' @inheritParams text_ref
#' @inheritParams singular_operators_ref
#' @inheritParams binary_operators_ref
#' @inheritParams valid_functions_ref
#' @inheritParams map_ref
#' @inheritParams mapping_names_ref
#'
#' @return numeric or logical vector
#'
#' @export
#'
#' @examples
#' eval_text("1 + 2")
#'
#' # using the map parameter 
#' map_obj <- list("#" = data.frame(x = 1:5, y = 5:1),"$" = list(z = -(1:5)))
#' y <- evalR::eval_text("#x# + $z$", map=map_obj)
eval_text <- function(text, singular_operators = NULL, binary_operators = NULL, valid_functions = NULL, map = NULL, mapping_names = NULL) {


  # if no map is passed, then set it as an empty list.
  if (is.null(map) == T) {
    map <- list()
  }


  # if singular_operators, binary_operators, or binary_operators are NULL, then let create_tree set the default values.

  # convert the text to a tree
  text_to_tree <- create_tree(text = text, singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions)

  # pass the tree into the eval tree function. 
  result_vector <- eval_tree(tree = text_to_tree, singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions, map = map, mapping_names = mapping_names)
  return(result_vector)
}







#' safely evaluate tree
#'
#' @description
#' Safe alternative to using eval + parse on some string that has already been converted into a tree.
#'
#' @details 
#' See \code{vignette("Overview", package = "evalR")}
#'
#' @inheritParams tree_ref
#' @inheritParams singular_operators_ref
#' @inheritParams binary_operators_ref
#' @inheritParams valid_functions_ref
#' @inheritParams map_ref
#' @inheritParams mapping_names_ref
#'
#' @return numeric or logical vector
#'
#' @export
#'
#' @examples
#' tree <- create_tree("1 + 2")
#' eval_tree(tree)
eval_tree <- function(tree, singular_operators = NULL, binary_operators = NULL, valid_functions = NULL, map = NULL, mapping_names = NULL) {
  if (is.list(tree) == F) {
    stop("tree is not a list")
  } else if (("eval" %in% names(tree)) == F) {
    stop("tree has no \"eval\" named element")
  } else if (("pval" %in% names(tree)) == F) {
    stop("tree has no \"pval\" named element")
  }


  # set default singular operators
  if (is.null(singular_operators)) {
    singular_operators <- default_singular_operators
  }

  # set default binary_operators
  if (is.null(binary_operators)) {
    binary_operators <- default_binary_operators
  }

  # set default valid_functions
  if (is.null(valid_functions)) {
    valid_functions <- default_valid_functions
  }


  # if no map is passed, then set it as an empty list.
  if (is.null(map) == T) {
    map <- list()
  }

  # generate all the potential referenced syntax names
  # this code might be a little slow. That's why passing in the vector a head of time will save time.
  if (is.null(mapping_names)) {
    mapping_names <- c()
    #  Growing the vector doesn't seem that much slower.
    #  See the microbenchmark code below
    for (map_i in names(map)) {
      if (is.matrix(map[[map_i]])) {
        mapping_names <- c(mapping_names, paste0(map_i, colnames(map[[map_i]]), map_i))
      } else {
        mapping_names <- c(mapping_names, paste0(map_i, names(map[[map_i]]), map_i))
      }
    }
  }



  pval_branch <- tree[["pval"]]
  eval_branch <- tree[["eval"]]

  # evaluate the tree
  result_vector <- eval_tree_inner(tree = eval_branch, singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions, map = map, mapping_names = mapping_names, pval = pval_branch)
  return(result_vector)
}
#  microbenchmark::microbenchmark({
#   n=30
#   mapping_names <- c()
#   for(i in 1:n){
#     mapping_names <- c(mapping_names, 1:i)
#   }
# },{
#   n=30
#   mapping_names_l <- vector("list",n)
#   for(i in 1:n){
#     mapping_names_l[[i]] <- 1:i
#   }
#   mapping_names <- unlist(mapping_names_l)
#   })





#' build_function_parameter_list
#'
#' @description
#' Function gets called recursively to figure out how many parameters a function has.
#'
#' @param parenthesis_block_eval the eval branch of a parenthesis block of a function.
#' @param passed_list this is the list of parameters that will be passed to \code{do.call}. This function will recursively add function parameters in the right order.
#' @inheritParams binary_operators_ref
#' @inheritParams valid_functions_ref
#' @inheritParams map_ref
#' @inheritParams mapping_names_ref
#' @inheritParams pval_ref
#'
#' @return numeric or logical vector 
#'
#' @noRd

build_function_parameter_list <- function(parenthesis_block_eval, passed_list, singular_operators, binary_operators, valid_functions, map, mapping_names, pval) {

  if (parenthesis_block_eval[[1]] == ",") { 

    # if the first element is "," , then we know elements 2 and 3 need to be passed into build_function_parameter_list

    passed_list <- build_function_parameter_list(parenthesis_block_eval[[2]],
      passed_list = passed_list,
      singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions,
      map = map, mapping_names = mapping_names, pval = pval
    )
    passed_list <- build_function_parameter_list(parenthesis_block_eval[[3]],
      passed_list = passed_list,
      singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions,
      map = map, mapping_names = mapping_names, pval = pval
    )

  } else { 

    # since the first element is not ",", then treat parenthesis_block_eval as a tree to evaluate.

    passed_list[[length(passed_list) + 1]] <- eval_tree_inner(parenthesis_block_eval,
      singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions,
      map = map, mapping_names = mapping_names, pval = pval
    )
  }
  return(passed_list)
}







#' eval_tree_inner
#'
#' @description
#' inner workhorse for evaluation of the tree
#'
#' @inheritParams tree_ref
#' @inheritParams singular_operators_ref
#' @inheritParams binary_operators_ref
#' @inheritParams valid_functions_ref
#' @inheritParams map_ref
#' @inheritParams mapping_names_ref
#' @inheritParams pval_ref
#'
#' @return numeric or logical vector
#'
#' @noRd

eval_tree_inner <- function(tree, singular_operators, binary_operators, valid_functions, map, mapping_names, pval) {
  first_element <- tree[[1]]
  if (first_element == "atomic") {
    return(
      eval_tree_atomic(
        tree = tree,
        singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions,
        map = map, mapping_names = mapping_names, pval = pval
      )
    )
  }else if (length(tree) == 2) {

    # nodes of length two that are not atomic 
    # these are either singular operator nodes or valid function nodes.

    if (first_element %in% singular_operators) {

      pass_list <- list(
        eval_tree_inner(tree[[2]],
          singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions,
          map = map, mapping_names = mapping_names, pval = pval
        )
      )

      return(do.call(first_element, pass_list))

    } else if (first_element %in% valid_functions) {

      # quick assertions
      # function call should have been wrapped in parenthesis. Hence, first element needs to be an atomic parenthesis reference
      if (tree[[2]][[1]] != "atomic") {
        stop(paste0(tree[[1]], " pattern unexpected"))
      }else if((tree[[2]][[2]] %in% names(pval))==F){
        stop(paste0(tree[[1]], " element not a key to pval"))
      }

      # parenthesis reference. (already confirmed first element is atomic)
      parenthesis_block_name <- tree[[2]][[2]]
      # get parenthesis reference tree
      parenthesis_block_tree <- pval[[parenthesis_block_name]]

      # create the parameter list for do.call
      passed_list <- build_function_parameter_list(
        parenthesis_block_eval = parenthesis_block_tree[["eval"]], passed_list = list(),
        singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions,
        map = map, mapping_names = mapping_names, pval = parenthesis_block_tree[["pval"]]
      )

      return(do.call(first_element, passed_list))
    }

      
    stop("tree not recognized")
    
  } else if (length(tree) == 3) {

    # only tree nodes that have length 3 will be binary_operators nodes

    if (first_element %in% binary_operators) {

      # nodes at this stage are not being handled as part of a function evaluation
      # Hence, values separated by "," are assumed to be needing to be combined together.
      if (first_element == ",") {
        first_element <- "c"
      }

      # create the parameter list for do.call
      pass_list <- list(
        eval_tree_inner(tree[[2]],
          singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions,
          map = map, mapping_names = mapping_names, pval = pval
        ),
        eval_tree_inner(tree[[3]],
          singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions,
          map = map, mapping_names = mapping_names, pval = pval
        )
      )
      return(do.call(first_element, pass_list))
    }
        
    
    stop("tree not recognized")

  }


  # should never make it here
  stop("tree not recognized")
  return(0)
}



#' eval_tree_atomic
#'
#' @description
#' extract out the logic for evaluating atomic nodes. Makes it easier to test.
#'
#' @inheritParams tree_ref
#' @inheritParams singular_operators_ref
#' @inheritParams binary_operators_ref
#' @inheritParams valid_functions_ref
#' @inheritParams map_ref
#' @inheritParams mapping_names_ref
#' @inheritParams pval_ref
#'
#' @return numeric or logical vector
#'
#' @noRd

eval_tree_atomic <- function(tree, singular_operators, binary_operators, valid_functions, map, mapping_names, pval) {


  second_element <- tree[[2]]

  if(is.character(second_element)==F){
    stop("atomic node has 2nd element that is not a string.")
  }

  # The assumption is that the key is only one char long.
  first_char <- substr(second_element, 1, 1)

  if (first_char == "\\") {
    if (second_element %in% names(pval)) {

      # Evaluate the statement that was inside parenthesis. This is like evaluating a whole new statement.
      parenthesis_block_val <- eval_tree(pval[[second_element]],
        singular_operators = singular_operators, binary_operators = binary_operators, valid_functions = valid_functions,
        map = map, mapping_names = mapping_names
      )

      return(parenthesis_block_val)
    } else {
      stop(paste0("Unknown atomic node 2nd element (",second_element,")."))
    }
  } else if (second_element %in% mapping_names) {

    # current map element
    map_i <- map[[first_char]]

    # it's assumed the first and last elements will be the key.
    ref_name <- substr(second_element, 2, nchar(second_element) - 1)
    if (is.data.frame(map_i) || is.matrix(map_i)) {
      return(as.numeric(map_i[, ref_name, drop = T]))
    } else if (is.list(map_i)) {
      return(as.numeric(map_i[[ref_name]]))
    } else {
      stop(paste0("map element (",first_char,") has unknown type. Expected to be either matrix, data.frame, or list"))
    }
  } else if (second_element %in% c("T", "TRUE", "F", "FALSE")) {
    return(as.logical(second_element))
  } else {
    # everything else will be cast to numeric
    return(as.numeric(second_element))
  }
}
