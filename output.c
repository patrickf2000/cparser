int add() {
    int answer;
    answer = x + y;
}

int main() {
    int no;
    no = 20;
    no = add(no, 23);
    no = add(no, 44) + 77;
    print_int(no);
    add(no, no * 27);
}

