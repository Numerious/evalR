// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include <Rcpp.h>
#include <regex>






// trim from start (in place)
// Code borrowed from https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring
static inline void ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    }));
}

// trim from end (in place)
// Code borrowed from https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring
static inline void rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

// trim from both ends (in place)
// Code borrowed from https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring
static inline void trim(std::string &s) {
    ltrim(s);
    rtrim(s);
}


// trim from both ends (copying)
// Code borrowed from https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring
static inline std::string trim_copy(std::string s) {
    trim(s);
    return s;
}

// replace every instance of substd::string
// Code borrowed from https://stackoverflow.com/questions/1494399/how-do-i-search-find-and-replace-in-a-standard-string
void replace_all(
    std::string& s,
    std::string const& toReplace,
    std::string const& replaceWith
) {
    std::string buf;
    std::size_t pos = 0;
    std::size_t prevPos;

    // Reserves rough estimate of final size of string.
    buf.reserve(s.size());

    while (true) {
        prevPos = pos;
        pos = s.find(toReplace, pos);
        if (pos == std::string::npos)
            break;
        buf.append(s, prevPos, pos - prevPos);
        buf += replaceWith;
        pos += toReplace.size();
    }

    buf.append(s, prevPos, s.size() - prevPos);
    s.swap(buf);
}


//' rcpp_find_parenthesis
//'
//' @description See R function find_parenthesis
//' @inheritParams text_ref
//' @return a std::string
//' @noRd
// [[Rcpp::export]]
std::string rcpp_find_parenthesis(std::string const &text)
{

    // this scalar values are used to track parenthesis
    int p_start = -1;
    int p_end = -1;
    int ps_count = 0;
    int p_count = 0;
    std::size_t found = text.find("("); // check if a parenthesis is included in the text
    if (found != std::string::npos)
    {

        // the idea is to loop of each char and count how many parenthesis are encountered. 
        // Once the closing parenthesis of the first opening parenthesis has been found, then break out of the loop.
        for (unsigned int i = 0; i < text.length(); i++) // walk over each char
        {
            std::string x = text.substr(i, 1);
            if (x == "(") // check if element is an opening parenthesis
            {
                ps_count += 1;
                p_count += 1;
            }
            else if (x == ")") // check if element is an closing parenthesis
            {
                p_count -= 1;
            }

            if (ps_count == 1 && p_start == -1) // check if this is the first time we encountered an opening parenthesis
            {
                p_start = i;
            }
            else if (ps_count > 0 && p_count == 0 && p_end == -1) // check if we found the closing parenthesis of the first opening parenthesis
            {
                p_end = i;
                break;
            }
        } //  for (unsigned int i = 0; i < text.length(); i++)

        if ((p_start >= 0) & (p_end >=0)) // make sure we found a opening parenthesis and closing
        {
            return text.substr(p_start, p_end - p_start + 1);
        }
        else // no opening and closing parenthesis found.
        {
            return "";
        }
    }
    else
    { // no parenthesis is found. Hence, return ""
        return "";
    }
}


//' helper to convert std::string to tree
//'
//' @description inner work horse for rcpp_create_tree
//' @inheritParams text_ref
//' @inheritParams singular_operators_ref
//' @inheritParams binary_operators_ref
//' @inheritParams valid_functions_ref
//' @return a Rcpp::List
//' @noRd
Rcpp::List eval_tree(std::string &text, std::vector<std::string> const &singular_operators, std::vector<std::string> const &binary_operators, std::vector<std::string> const &valid_functions)
{

    // trim whitespace around the text
    trim(text);
    int nstr = text.length();


    if (nstr == 0) // no text string should be length 0
    {
        Rcpp::stop("'text' is of length 0.");
    }
    else if (nstr == 1) // by definition, a string of length 1 must be an atomic element
    {
        Rcpp::List return_list = Rcpp::List::create("atomic", text);
        return return_list;
    }

    if (nstr > 2) // for a binary operators to be include, the string must be at least 3 characters long 
    {
        int vector_size = binary_operators.size();

        
        for (int char_i = 0; char_i < vector_size; char_i++) // loop over every element of binary operators to see if any exist. Note the order of precedence

        {
            std::string element_i = binary_operators[char_i];

            // test if this operator can be found in the text.
            std::size_t found = text.find(element_i); 
            if (found != std::string::npos)
            {



                int element_i_length = element_i.length();

                for (int i = 1; i < nstr - element_i_length + 1; i++) // Walking over the vector to see where the operator text starts.

                {
                    std::string subs = text.substr(i, element_i_length);
                    if (subs == element_i)
                    {

                        // This node of the tree will have 3 elements
                        // 1 - the binary operator
                        // 2 - left side value
                        // 3 - right side value
                        std::string start_string = trim_copy(text.substr(0, i));
                        std::string end_string = trim_copy(text.substr(i + element_i_length, nstr - (i + element_i_length)));

                        Rcpp::List return_list = Rcpp::List::create(element_i, 
                        eval_tree(start_string, singular_operators, binary_operators, valid_functions), 
                        eval_tree(end_string, singular_operators, binary_operators, valid_functions)
                        );
                        return return_list;
                    } // if (subs == element_i)
                } // for (int i = 1; i < nstr - element_i_length + 1; i++)
            } // if (found != std::string::npos)
        } // for (int char_i = 0; char_i < vector_size; char_i++)
    } // if (nstr > 2) 

    int vector_size = singular_operators.size();
    for (int char_i = 0; char_i < vector_size; char_i++) // loop over every element of singular operators to see if any exist.

    {

        if (text.substr(0, 1) == singular_operators[char_i]) // only checking if the first char matches the operator. Note the text has been trimmed.
        {


            // This node of the tree will have 2 elements
            // 1 - the singular operator
            // 2 - right side value
            std::string end_string = trim_copy(text.substr(1, nstr - 1));

            Rcpp::List return_list = Rcpp::List::create(singular_operators[char_i], 
            eval_tree(end_string, singular_operators, binary_operators, valid_functions)
            );
            return return_list;
        } // if (text.substr(0, 1) == singular_operators[char_i])
    } // for (int char_i = 0; char_i < vector_size; char_i++)
    if (nstr > 2)
    {
        vector_size = valid_functions.size();
        for (int char_i = 0; char_i < vector_size; char_i++) // loop over every element of valid functions to see if any exist.

        {
            std::string element_i = valid_functions[char_i];


            // the grammer assumes that any valid function will require parenthesis to invoke. 
            // Hence, we are check that the function is followed by \.
            // This has to be true because all parenthesis blocks have already been replace by the \\\\[0-9]+ pattern. 
            std::size_t found = text.find(element_i+"\\"); // do the quick non-regex search first
            if (found != std::string::npos)
            {
                int element_i_length = element_i.length();

                std::smatch m1;
                std::regex e1("^" + element_i + "\\\\[0-9]+"); 

                if (std::regex_search(text, m1, e1)) // do the better regex search to confirm the match
                {


                    // This node of the tree will have 2 elements
                    // 1 - the valid function text
                    // 2 - the parenthesis block lookup token
                    std::string end_string = trim_copy(text.substr(element_i_length, nstr - element_i_length));

                    Rcpp::List return_list = Rcpp::List::create(element_i, 
                    eval_tree(end_string, singular_operators, binary_operators, valid_functions)
                    );
                    return return_list;
                } // if (std::regex_search(text, m1, e1))
            } // if (found != std::string::npos)
        } // for (int char_i = 0; char_i < vector_size; char_i++)
    } // if (nstr > 2)


    // if the text doesn't match any other pattern, then return it as an atomic node.
    // in the eval functions, this will be cast to either a logical or numeric value.
    Rcpp::List return_list = Rcpp::List::create("atomic", text);
    return return_list;
}


//' Convert a statement into an evaluation tree
//'
//' @description See create_tree R function
//' @inheritParams text_ref
//' @inheritParams singular_operators_ref
//' @inheritParams binary_operators_ref
//' @inheritParams valid_functions_ref
//' @return a Rcpp::List
//' @noRd
// [[Rcpp::export]]
Rcpp::List rcpp_create_tree(std::string text, std::vector<std::string> const &singular_operators, std::vector<std::string> const &binary_operators, std::vector<std::string> const &valid_functions)
{

  // each tree is made up of two branches.
  // `pval` - stands for parenthesis values
  // `eval` - stands for evaluation values

  // start - create the pval branch

  Rcpp::List pval_branch = Rcpp::List::create();

  // check to see if any parenthesis block can be found.
  std::string parenthesis_block = rcpp_find_parenthesis(text);
  while (parenthesis_block != "") // keep looping until no new block can be found.
  {
    // item_i will be a unique index for this element
    int item_i = pval_branch.length();

    // pval_name is the name for this pval element and what will be used to replace the parenthesis block in the text.
    std::string pval_name = "\\" + std::to_string(item_i);

    // remove parenthesis and trim the inner substring.
    std::string trim_parenthesis_block = trim_copy(parenthesis_block.substr(1, parenthesis_block.length() - 2));

    // treat the inner substring like a new statement that needs to be converted into a tree.
    pval_branch[pval_name] = rcpp_create_tree(trim_parenthesis_block, singular_operators, binary_operators, valid_functions);

    // replace the parenthesis block with the pval element name
    replace_all(text, parenthesis_block, pval_name);

    // check again to see if another parenthesis block can be found.
    parenthesis_block = rcpp_find_parenthesis(text);
  } // while (parenthesis_block != "")

  // end - create the pval branch

  // start - create the eval branch

  // all parenthesis blocks have been replace. Now create the remaining statement into a tree
  Rcpp::List eval_branch = eval_tree(text, singular_operators, binary_operators, valid_functions);

  // end - create the eval branch

  // create the tree structure

  Rcpp::List tree = Rcpp::List::create();
  tree["pval"] = pval_branch;
  tree["eval"] = eval_branch;

  return tree;
}
