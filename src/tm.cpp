// #include <Rcpp.h>
// #include <map>
// #include <iostream>
// #include <iterator>
// #include <algorithm>
// #include <string> 
// #include <fstream>
// #include <iomanip>
// #include <locale>
// #include <vector>
// 
// // [[Rcpp::plugins("cpp11")]]
// 
// struct letter_only: std::ctype<char> 
// {
//   letter_only(): std::ctype<char>(get_table()) {}
//   
//   static std::ctype_base::mask const* get_table()
//   {
//     static std::vector<std::ctype_base::mask> 
//     rc(std::ctype<char>::table_size,std::ctype_base::space);
//     
//     std::fill(&rc['A'], &rc['z'+1], std::ctype_base::alpha);
//     return &rc[0];
//   }
// };
// 
// struct Counter
// {
//   std::map<std::string, int> wordCount;
//   void operator()(const std::string & item) { ++wordCount[item]; }
//   operator std::map<std::string, int>() { return wordCount; }
// };
// 
// // [[Rcpp::export]]
// std::map<std::string, int> GetTermVectorCpp(std::string filename)
// {
//   std::ifstream input;
//   input.imbue(std::locale(std::locale(), new letter_only())); //enable reading only letters!
//   input.open(filename);
//   std::istream_iterator<std::string> start(input);
//   std::istream_iterator<std::string> end;
//   std::map<std::string, int> wordCount = std::for_each(start, end, Counter());
//   // for (std::map<std::string, int>::iterator it = wordCount.begin(); it != wordCount.end(); ++it)
//   // {
//   //   std::cout << it->first <<" : "<< it->second << std::endl;
//   // }
//   
//   return wordCount;
// }




#include <iterator>
#include <iostream>
#include <fstream>
#include <map>
#include <string>
#include <cctype>

// [[Rcpp::plugins("cpp11")]]

std::string getNextToken(std::istream &in)
{
  char c;
  std::string ans="";
  c=in.get();
  while(!std::isalpha(c) && !in.eof())//cleaning non letter charachters
  {
    c=in.get();
  }
  while(std::isalpha(c))
  {
    ans.push_back(std::tolower(c));
    c=in.get();
  }
  return ans;
}

//[[Rcpp::export]]
std::map<std::string, int> GetTermVectorCpp(std::string filename)
{
  std::map<std::string,int> words;
  std::ifstream fin(filename);
  
  std::string s;
  std::string empty ="";
  while((s=getNextToken(fin))!=empty )
    ++words[s];
  
  // for(std::map<std::string,int>::iterator iter = words.begin(); iter!=words.end(); ++iter)
  //   std::cout<<iter->first<<' '<<iter->second<<std::endl;
  
  return words;
}




