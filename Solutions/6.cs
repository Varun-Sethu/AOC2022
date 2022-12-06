using System;
using System.Linq;
using System.IO;
using System.Collections.Generic;

class Program {
    static int getSignalComponentEnd(int componentSize, List<char> signal) {
        var componentStart = signal.Select((_, i) => signal.Skip(i).Take(componentSize))
                                   .Where(window => window.Count() == componentSize)
                                   .TakeWhile(window => window.Distinct().Count() != componentSize)
                                   .Count();

        return componentSize + componentStart;
    }

    static int getMarkerLocationEnd(string signal) => getSignalComponentEnd(componentSize: 4, signal.ToList());
    static int getMessageLocationEnd(string signal) => getSignalComponentEnd(componentSize: 14, signal.ToList());

    public static void Main(string[] args) {
        var signal = File.ReadAllText("day6.txt");
        Console.WriteLine($"Part One: {getMarkerLocationEnd(signal)}");
        Console.WriteLine($"Part Two: {getMarkerLocationEnd(signal)}");
    }
}