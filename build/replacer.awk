# Tool to replace content between below cut-marks with content of argument file
# pointed to by awk parameter `to_replace`.

/MARK: CUT START/ {
    cutting = 1;
}

{
    if (!cutting) {
        print $0
    }
}

/MARK: CUT END/ {
    cutting = 0;
    while ((getline line < to_replace) > 0) {
        print line
    }
}
