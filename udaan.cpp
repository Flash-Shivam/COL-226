#include <iostream>
#include <cstdlib>
#include <string.h>
#include <vector>
using namespace std;
#define lld long long int

int main() {
	// your code goes here
	lld n,i,j;
	cin >> n;
	string input1,input2;
	pair <lld,lld> dim[n];
	pair <lld , 	vector<vector<bool> > > reserve[n];
	pair <lld, vector <lld> > aisle[n];
	lld r = 0;
	pair <string,lld> name[n];
	for(i=0;i<n;i++)
	{

		cin >> input1 >> input2;
	//	cout << input1 << "  --   " << input2 << endl;
		string input;
		getline(cin, input);
	//	cout << input << endl;
		lld q=0,a[1000];
		lld k = input.length();
		string x  = "";
		for(j=1;j<k;j++)
		{
			if(input[j]!=' ')
			{
				x = x + input[j];
			}
			else{
				a[q] = stoi(x);
				x = "";
				q++;
			}
		}

		a[q] = stoi(x);
		q++;

		lld flag = 0;
		if(input1 == "add-screen")
		{
			name[r].first = input2;
			name[r].second = r;
			dim[r].first = a[0];
			dim[r].second = a[1];
			vector<vector<bool> > f( a[0]+1 , vector<bool> (a[1]+1));


			for(j = 0;j<=a[0] ;j++)
			{
				for(k=0;k<=a[1];k++)
				{
					f[i][j] = false;
				}
			}

			vector <lld> y;
			for(j=2;j<q;j++)
			{
				y.push_back(a[j]);
			}

			aisle[r].first = r;
			aisle[r].second = y;
			reserve[r].first = r;
			reserve[r].second = f;

			cout << "success\n";
			r++;
		}
		else if(input1 == "reserve-seat")
		{
		//	cout << "IN  ";
			for(j=0;j<r;j++)
			{
				if(name[j].first == input2)
				{
					break;
				}
			}

			if(j==r||a[0]>dim[j].first||a[0]<1)
			{
				cout <<"failure\n";
				flag = 1;
			}
			else{
				for(k=1;k<q;k++)
				{
					if(a[k]>dim[j].second||a[k]<1)
					{
						cout <<"failure\n";
						flag = 1;
						break;
					}
				}

				if(k==q)
				{
		//			cout << "Found\n";

					for(k=1;k<q;k++)
					{
						if(reserve[j].second[a[0]][a[k]]==true)
						{
							cout <<"failure\n";
							flag = 1;
							break;
						}
					}

					if(k==q)
          {
						for(k=1;k<q;k++)
					{
						reserve[j].second[a[0]][a[k]]=true;
					}

					cout <<"success\n";
					flag = 1;

					}
				}


			}
		}
		else if(input1 == "get-unreserved-seats")
		{
			for(j=0;j<r;j++)
			{
				if(name[j].first == input2)
				{
					break;
				}
			}

			if(j==r||a[0]>dim[j].first||a[0]<1)
			{
				cout <<"failure\n";
				flag = 1;
			}
			else{
				lld g = dim[j].second;
		//	cout << "In  " << g << " " << j << endl;
				for(k=1;k<=g;k++)
				{
					if(reserve[j].second[a[0]][k]==false)
					{
						cout << k << " ";
					}
				}

				cout << endl;
			}
		}
		else{

					for(j=0;j<r;j++)
			{
				if(name[j].first == input2)
				{
					break;
				}
			}

			if(j==r||a[1]>dim[j].first||a[1]<1)
			{
				cout <<"none\n";
				flag = 1;
			}
			else{
			//	cout << "IN\n";
					flag = 1;
					lld aa = a[2]-a[0]+1;
					lld b = a[2];
			//	cout << aa << "  " << b << endl;
					if(aa>=1&&b<=dim[j].second)
					{
						for(k=0;k<aisle[j].second.size();k++)
						{
							if(aisle[j].second[k]>=aa+1&&aisle[j].second[k]<=b-1)
							{
								flag = 1;
								break;
							}
						}
					//	cout << "Found " << k << " " <<aisle[j].second.size() << " " ;
						if(k==aisle[j].second.size())
						{
						//	cout << "check ";
							for(k=aa;k<=b;k++)
							{
								if(reserve[j].second[a[1]][k]==true)
								{
									flag = 1;
									break;
								}
							}

						//	cout << k << endl;
						}

						if(k == b+1)
						{
							for(k=aa;k<=b;k++)
							{
								cout << k << " ";

							}
							cout << endl;

							flag = 0;
						}



					}

					 aa = a[2];
				     b = a[2]+a[0]-1;


					if(aa>=1&&b<=dim[j].second&&flag==1)
					{
						for(k=0;k<aisle[j].second.size();k++)
						{
							if(aisle[j].second[k]>=aa+1&&aisle[j].second[k]<=b-1)
							{
								flag = 1;
								break;
							}
						}

						if(k==aisle[j].second.size())
						{
							for(k=aa;k<=b;k++)
							{
								if(reserve[j].second[a[1]][k]==true)
								{
									flag = 1;
									break;
								}
							}
						}

						if(k == b+1)
						{
							for(k=aa;k<=b;k++)
							{
								cout << k << " ";

							}
							cout << endl;
							flag = 0;

						}



					}

					if(flag==1)
					{
						cout << "none\n";
					}

			}



		}

	//	cout << "Done " << i << endl;

	}
	return 0;
}
