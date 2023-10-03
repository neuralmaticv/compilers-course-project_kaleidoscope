#include <iostream>

extern "C"
{
    double avg(double, double);
}

int main()
{
    std::cout << "average of 5.0 and 10.0: " << avg(5.0, 10.0) << std::endl;
}
