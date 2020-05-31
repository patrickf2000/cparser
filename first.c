int main() {
	int x = 20;
	int y = 30 + x * 2;
	
	x = 30;
	swap(20);
	swap(2 * y, x);
	
	x = 2 * swap(99) + 9;
	
	return 7;
}

int swap() {
    int z = 20;
    return z;
}
