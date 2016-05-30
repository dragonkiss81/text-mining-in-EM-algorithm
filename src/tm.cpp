#include <Rcpp.h>
#include <map>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <string> 
#include <fstream>
#include <iomanip>
#include <locale>
#include <vector>

// [[Rcpp::plugins("cpp11")]]

struct letter_only: std::ctype<char> 
{
  letter_only(): std::ctype<char>(get_table()) {}
  
  static std::ctype_base::mask const* get_table()
  {
    static std::vector<std::ctype_base::mask> 
    rc(std::ctype<char>::table_size,std::ctype_base::space);
    
    std::fill(&rc['A'], &rc['z'+1], std::ctype_base::alpha);
    return &rc[0];
  }
};

struct Counter
{
  std::map<std::string, int> wordCount;
  void operator()(const std::string & item) { ++wordCount[item]; }
  operator std::map<std::string, int>() { return wordCount; }
};

// [[Rcpp::export]]
std::map<std::string, int> GetTermVectorCpp(std::string filename)
{
  std::ifstream input;
  input.imbue(std::locale(std::locale(), new letter_only())); //enable reading only letters!
  input.open(filename);
  std::istream_iterator<std::string> start(input);
  std::istream_iterator<std::string> end;
  std::map<std::string, int> wordCount = std::for_each(start, end, Counter());
  // for (std::map<std::string, int>::iterator it = wordCount.begin(); it != wordCount.end(); ++it)
  // {
  //   std::cout << it->first <<" : "<< it->second << std::endl;
  // }
  
  return wordCount;
}


