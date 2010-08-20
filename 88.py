psna = {}

def mpsn(k):
    print k
    n = 2
    while True:
        if psn(k, n):
            return n
        n += 1
def psn(k, n):
    return psn2(k, n, n)
def psn2(k, p, s):
    #print (k,p,s)
    if k == 1 and p != s:
        psna[(k,p,s)] = False
        return False
    if s == p + k - 1:
        psna[(k,p,s)] = True
        return True
    if s > p:
        psna[(k,p,s)] = False
        return False
    for d in divisors(p):
        key = (k-1, p // d, s - d)
        if key in psna:
            result = psna[key]
        else:
            result = psn2(*key)
        psna[key] = result
        if result:
            return result
    return False

def divisors(n):
    return [d for d in range(n-1,1,-1) if n % d == 0]
    
set([4, 6, 8, 9, 12, 15, 16, 18, 20, 24, 25, 28, 30, 32, 35, 36, 40, 42, 45, 48, 49, 54, 56, 60, 63, 64, 66, 70, 72, 76, 77, 80, 81, 84, 88, 90, 96, 98, 99, 100, 104, 108, 110, 112, 114, 117, 120, 121, 124, 126, 128, 130, 132, 135, 136, 140, 143, 144, 148, 150, 152, 153, 154, 156, 160, 162, 165, 168, 169, 176, 180, 182, 186, 187, 190, 192, 195, 196, 198, 200, 204, 208, 210, 216, 220, 221, 222, 224, 225, 228, 231, 234, 238, 240, 247, 248, 252, 255, 256, 260, 264, 266, 270, 272, 276, 280, 285, 286, 288, 289, 294, 296, 299, 300, 304, 306, 308, 310, 312, 315, 316, 320, 322, 323, 324, 328, 330, 336, 340, 342, 345, 348, 350, 352, 357, 360, 361, 364, 368, 370, 372, 374, 378, 380, 384, 388, 390, 391, 392, 396, 399, 400, 405, 408, 414, 416, 418, 420, 424, 425, 432, 434, 435, 437, 440, 441, 444, 448, 450, 456, 459, 460, 462, 468, 474, 475, 476, 480, 483, 484, 486, 490, 492, 493, 494, 496, 500, 504, 506, 510, 512, 513, 516, 518, 520, 522, 525, 528, 529, 532, 540, 544, 546, 550, 552, 556, 558, 560, 561, 564, 567, 568, 570, 572, 575, 576, 580, 582, 588, 592, 594, 598, 600, 608, 609, 612, 616, 620, 621, 624, 625, 628, 630, 632, 636, 638, 640, 644, 646, 648, 650, 656, 660, 664, 665, 666, 667, 672, 675, 676, 680, 682, 684, 686, 690, 693, 696, 700, 702, 704, 708, 713, 714, 720, 722, 725, 726, 728, 729, 735, 736, 740, 744, 748, 750, 754, 756, 759, 760, 768, 770, 774, 775, 776, 780, 782, 783, 784, 790, 792, 796, 798, 800, 804, 805, 806, 810, 812, 814, 816, 819, 820, 825, 828, 832, 834, 836, 837, 840, 841, 844, 846, 848, 850, 852, 858, 864, 868, 870, 874, 875, 880, 882, 884, 888, 891, 896, 897, 899, 900, 910, 912, 916, 918, 920, 924, 928, 930, 936, 942, 943, 945, 946, 948, 950, 952, 954, 957, 960, 961, 962, 966, 968, 970, 972, 975, 976, 980, 984, 986, 988, 990, 992, 996, 999, 1000, 1008, 1012, 1014, 1015, 1020, 1023, 1024, 1026, 1032, 1035, 1036, 1040, 1044, 1048, 1050, 1053, 1054, 1056, 1060, 1062, 1064, 1078, 1080, 1084, 1088, 1092, 1096, 1100, 1104, 1106, 1112, 1116, 1120, 1122, 1128, 1136, 1140, 1144, 1148, 1150, 1152, 1156, 1164, 1170, 1176, 1188, 1194, 1200, 1204, 1206, 1210, 1216, 1228, 1230, 1232, 1236, 1240, 1248, 1256, 1260, 1264, 1266, 1272, 1284, 1288, 1296, 1302, 1308, 1320, 1324, 1328, 1332, 1348, 1350, 1356, 1368, 1374, 1384, 1386, 1392, 1404, 1420, 1440, 1456, 1464, 1468, 1476, 1480, 1488, 1500, 1504, 1516, 1524, 1540, 1548, 1576, 1596, 1620, 1624, 1644, 1648, 1656, 1660, 1680, 1708, 1716, 1720, 1728, 1756, 1764, 1768, 1776, 1816, 1824, 1840, 1860, 1876, 1884, 1896, 1908, 1936, 1944, 1956, 1968, 1984, 1996])