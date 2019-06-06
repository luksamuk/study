// Parte 1
unsigned long int code;
string            name;
string            author;
string            publisher;
size_t            num_pages;

// Parte 2
cout << this->code      << endl
     << this->name      << endl
     << this->author    << endl
     << this->publisher << endl
     << this->num_pages << endl;

// Parte 3
cin >> this->code;
while(cin.peek() == '\n')
    cin >> ws;
getline(cin, this->name);
getline(cin, this->author);
getline(cin, this->publisher);
cin >> this->num_pages;
while(cin.peek() == '\n')
    cin >> ws;
