package algorithms;

import com.sun.deploy.util.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Created by markmo on 4/05/2014.
 */
public class InversionCount {

    public MutableTuple<List<Integer>, Long> countInversions(List<Integer> a) {
        return countInversions(a, a.size());
    }

    public MutableTuple<List<Integer>, Long> countInversions(List<Integer> a, int n) {
        if (n == 1) {
            return new MutableTuple<>(a, 0L);
        }
        int l = n / 2;
        int m = n - l;
        MutableTuple<List<Integer>, Long> b = countInversions(a.subList(0, l), l);
        MutableTuple<List<Integer>, Long> c = countInversions(a.subList(l, n), m);
        MutableTuple<List<Integer>, Long> d = countSplitInversions(b._1, c._1, new MutableTuple<>(new ArrayList<>(), 0L));
        return new MutableTuple<>(d._1, b._2 + c._2 + d._2);
    }

    private MutableTuple<List<Integer>, Long> countSplitInversions(List<Integer> left, List<Integer> right, MutableTuple<List<Integer>, Long> acc) {
        if (left.isEmpty() && right.isEmpty()) {
            return acc;
        }
        if (right.isEmpty()) {
            acc._1.addAll(left);
            return acc;
        }
        if (left.isEmpty()) {
            acc._1.addAll(right);
            return acc;
        }
        int x = left.get(0);
        int y = right.get(0);
        if (x < y) {
            acc._1.add(x);
            return countSplitInversions(left.subList(1, left.size()), right, acc);
        } else {
            acc._1.add(y);
            acc._2 += left.size();
            return countSplitInversions(left, right.subList(1, right.size()), acc);
        }
    }

    public class MutableTuple<X, Y> {
        public X _1;
        public Y _2;
        public MutableTuple(X x, Y y) {
            this._1 = x;
            this._2 = y;
        }
    }

    private static void testCountInversions() {
        InversionCount ic = new InversionCount();
        MutableTuple<List<Integer>, Long> x = ic.countInversions(Arrays.asList(6, 5, 4, 3, 2, 1));
        Stream<String> strings = x._1.stream().map(String::valueOf);
        System.out.println(strings.collect(Collectors.joining(",")));
        System.out.println(x._2);
        x = ic.countInversions(Arrays.asList(4, 80, 70, 23, 9, 60, 68, 27, 66, 78, 12, 40, 52, 53, 44, 8, 49, 28, 18, 46, 21, 39, 51, 7, 87, 99, 69, 62, 84, 6, 79, 67, 14, 98, 83, 0, 96, 5, 82, 10, 26, 48, 3, 2, 15, 92, 11, 55, 63, 97, 43, 45, 81, 42, 95, 20, 25, 74, 24, 72, 91, 35, 86, 19, 75, 58, 71, 47, 76, 59, 64, 93, 17, 50, 56, 94, 90, 89, 32, 37, 34, 65, 1, 73, 41, 36, 57, 77, 30, 22, 13, 29, 38, 16, 88, 61, 31, 85, 33, 54));
        strings = x._1.stream().map(String::valueOf);
        System.out.println(strings.collect(Collectors.joining(",")));
        System.out.println(x._2);
    }

    public static void main(String[] args) throws Exception {
        testCountInversions();
    }

}
