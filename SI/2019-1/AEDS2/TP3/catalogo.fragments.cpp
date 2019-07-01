// Parte 1
unsigned long int code;
string            name;
string            author;
string            publisher;
size_t            num_pages;

// Parte 2
cout << l1.code      << endl
     << l1.name      << endl
     << l1.author    << endl
     << l1.publisher << endl
     << l1.num_pages << endl;

// Parte 3
Livro book;
cin >> book.code;
while(cin.peek() == '\n')
    cin >> ws;
getline(cin, book.name);
getline(cin, book.author);
getline(cin, book.publisher);
cin >> book.num_pages;
while(cin.peek() == '\n')
    cin >> ws;
return book;
