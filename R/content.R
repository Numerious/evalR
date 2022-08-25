
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
#' @param text The string/code/statement you want to parse.
#'
#'
#' @name text_ref

NULL



#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param singular_operators Tokens of length 1 that operate on a right hand value. For example, the `-` token is an operator to negate a vector. \code{NULL} value will be replaced with \code{c("-", "!")}.
#'
#'
#' @name singular_operators_ref

NULL




#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param binary_operators Tokens of any length that operate on a left and right hand values. For example, the `+` token is an operator that adds a left vector to a right vector. \code{NULL} value will be replaced with \code{c(",", "|", "&", "<=", "<", ">=", ">", "==", "!=", "+", "-", "*", "\%/\%", "/", "\%\%", "\%in\%", ":", "^")}. The order determines the precedence of the operators.
#'
#'
#' @name binary_operators_ref

NULL





#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param valid_functions Tokens of any length that are prefixed on a parenthesis block and specify a function to run on the provided parameters within the block. For example, the `log` token will evaluate the logarithm value of the first parameter. Note named parameters are not support. \code{NULL} value will be replaced with \code{c("log", "c", "any", "all", "abs", "ifelse")}.
#'
#'
#' @name valid_functions_ref

NULL




#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param map A named list of data.frames/lists/matrices. Where names are keys for referencing the values in the \code{text} parameters.
#'
#'
#' @name map_ref

NULL




#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param mapping_names Optional argument to make the function faster or limit which map elements can be referenced.
#'
#'
#' @name mapping_names_ref

NULL





#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param tree The output object from \link{create_tree}
#'
#'
#' @name tree_ref

NULL



#' arguments to use with @inheritParams
#'
#' @keywords internal
#' @param pval The pval branch of a \code{tree}
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
#' @return A substring. Either "" or the first parenthesis block.
#'
#' @export
#'
#' @examples
#' # returns ""
#' find_parenthesis("3 + 5")
#' # returns "(3 + 5)"
#' find_parenthesis("2 * (3 + 5)")
find_parenthesis <- function(text){
    return(rcpp_find_parenthesis(text))
}


#' Convert a statement into an evaluation tree
#'
#' @description
#' Function will break \code{text} into a list of lists.
#'
#' @details 
#' See \code{vignette("Overview", package = "evalR")}
#'
#' @inheritParams text_ref
#' @inheritParams singular_operators_ref
#' @inheritParams binary_operators_ref
#' @inheritParams valid_functions_ref
#'
#' @return A list of lists. In other words, a tree data structure made from lists.
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


  # Call the rcpp version of the function.
  return(rcpp_create_tree(text, singular_operators, binary_operators, valid_functions))
}




#' Safely evaluate text
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
#' @return A numeric or logical vector
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







#' Safely evaluate tree
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
#' @return A numeric or logical vector
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
#' @param parenthesis_block_eval The eval branch of a parenthesis block of a function.
#' @param passed_list This is the list of parameters that will be passed to \code{do.call}. This function will recursively add function parameters in the right order.
#' @inheritParams binary_operators_ref
#' @inheritParams valid_functions_ref
#' @inheritParams map_ref
#' @inheritParams mapping_names_ref
#' @inheritParams pval_ref
#'
#' @return A numeric or logical vector 
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
#' @return A numeric or logical vector
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
#' @return A numeric or logical vector
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
