// Parte 1
unsigned int pass_number;
char         name[80];
char         date[11];
char         hour[6];
char         origin[80];
char         dest[80];
unsigned int seat;

// Parte 2
cout << p1.name        << endl
     << p1.pass_number << endl
     << p1.date        << endl
     << p1.hour        << endl
     << p1.origin      << endl
     << p1.dest        << endl
     << p1.seat        << endl;

// Parte 3
Passagem pass;
while(cin.peek() == '\n') cin >> ws;
cin.getline(pass.name, 80);
cin >> pass.pass_number;
while(cin.peek() == '\n') cin >> ws;
cin.getline(pass.date, 11);
cin.getline(pass.hour, 6);
cin.getline(pass.origin, 80);
cin.getline(pass.dest, 80);
cin >> pass.seat;
return pass;
