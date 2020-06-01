int add(int x, int y) {
    int answer = x + y;
    return answer;
}

int main() {
    int no = 20;
    no = add(no, 23);
    no = add(no, 44) + 77;
    
    print_int(no);
    add(no, no * 27);
    
    return 0;
}
