// Parte 1
unsigned int pass_number;
char         name[80];
char         date[11];
char         hour[6];
char         origin[80];
char         dest[80];
unsigned int seat;

// Parte 2
cout << this->name        << endl
     << this->pass_number << endl
     << this->date        << endl
     << this->hour        << endl
     << this->origin      << endl
     << this->dest        << endl
     << this->seat        << endl;

// Parte 3
while(cin.peek() == '\n') cin >> ws;
cin.getline(this->name, 80);
cin >> this->pass_number;
while(cin.peek() == '\n') cin >> ws;
cin.getline(this->date, 11);
cin.getline(this->hour, 6);
cin.getline(this->origin, 80);
cin.getline(this->dest, 80);
cin >> this->seat;


