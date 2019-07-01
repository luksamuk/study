#include <iostream>
#include <string>

/* TAD  */
typedef unsigned long int code_t;

struct book_t {
    code_t      code;
    std::string name;
    std::string author;
    std::string publisher;
    size_t      num_pages;
};


/* Overload de operadores */

// Caso o comando subsequente seja std::getline, e ele
// não for precedido de outro std::getline, é essencial
// garantir que não haja nenhum caractere de nova linha
// pendente no stream de entrada.
inline void
eat_newline_chars(std::istream& is)
{
    while(is.peek() == '\n')
        is >> std::ws;
}


std::istream&
operator>>(std::istream& is, book_t& book)
{
    is >> book.code;
    eat_newline_chars(is);
    std::getline(is, book.name);
    std::getline(is, book.author);
    std::getline(is, book.publisher);
    is >> book.num_pages;
    eat_newline_chars(is);
    return is;
}

std::ostream&
operator<<(std::ostream& os, const book_t& book)
{
    os << book.code      << std::endl
       << book.name      << std::endl
       << book.author    << std::endl
       << book.publisher << std::endl
       << book.num_pages << std::endl;
    return os;
}



/* Operações básicas */
void
print_book_data(book_t book)
{
    std::cout << book;
}

book_t
read_book_data(void)
{
    book_t book;
    std::cin >> book;
    return book;
}



/* Entry point */
int
main(void)
{
    int    num_books;
    book_t *books;
    
    std::cin >> num_books;

    if(num_books <= 0) return 0;

    books = new book_t[num_books];
    if(!books) return 1;

    for(int i = 0; i < num_books; i++) {
        books[i] = read_book_data();
    }
    
    for(int i = 0; i < num_books; i++) {
        std::cout << "Livro " << (i + 1) << std::endl;
        print_book_data(books[i]);
        std::cout << std::endl;
    }

    delete [] books;
    
    return 0;
}
