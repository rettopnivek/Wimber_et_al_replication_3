array <int> finalSeq[432] = {56,3,45,42,52,57,33,35,41,46,60,25,22,9,1,18,61,33,24,24,57,10,62,39,8,39,49,49,18,66,31,58,62,11,9,71,32,43,60,35,42,23,40,53,48,57,64,6,39,44,63,21,37,37,43,56,46,9,48,50,46,50,50,51,20,29,36,59,43,43,13,13,54,54,56,16,16,11,13,48,53,8,49,49,59,45,38,54,67,67,15,37,37,23,56,1,41,25,18,39,21,72,55,29,16,50,1,19,44,64,44,44,3,3,45,6,23,65,43,43,28,60,33,48,39,58,72,65,22,40,19,46,66,66,19,19,30,16,6,34,34,20,15,15,62,46,54,7,7,55,12,33,29,66,66,53,9,16,20,33,6,22,52,4,12,12,55,28,16,47,47,34,34,48,26,36,36,6,14,69,11,72,69,56,44,59,70,70,5,5,33,44,65,26,26,38,58,62,60,67,71,26,45,24,2,5,28,36,65,9,18,11,6,31,51,27,51,63,40,47,26,41,23,23,4,53,12,12,5,38,9,70,8,31,64,55,70,32,31,42,29,41,65,64,57,26,20,62,66,41,41,65,35,18,39,61,50,47,19,19,24,24,40,5,38,20,30,17,17,56,50,15,51,58,53,2,3,7,7,68,68,54,61,61,31,30,22,14,53,57,27,29,69,60,34,2,17,67,11,32,48,13,35,27,27,40,21,31,2,69,24,21,69,69,61,13,54,10,49,49,15,15,59,45,67,1,11,25,25,30,63,17,3,3,10,8,42,14,14,22,23,34,37,42,10,45,36,36,25,68,68,60,47,20,27,27,67,71,71,18,47,40,55,55,46,52,4,52,25,2,2,70,17,8,8,28,28,17,68,22,72,1,5,51,14,32,32,4,70,7,1,28,52,14,4,4,30,30,58,52,29,61,71,71,59,59,38,38,57,21,21,63,72,63,58,51,64,62,32,68,10,10,13,37,63,64,7,12,42,72,35,35};
array <int> buttons[432] = {0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,1,2,1,2,1,2,2,1,2,2,2,2,2,2,1,2,2,2,2,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,2,2,1,2,2,1,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,1,2,1,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,1,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,1,2,2,2,2,2,1,2,1,2,2,2,2,2,2,1,2,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,1,2,2,1,2,2,2,2,1,2,2,1,2,2,2,2,1,2,2,2,2,2,2,1,2,2,2,1,2,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,1,2,1,2,2,2,2,2,1,2,1,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,0,0};
array <int> nullEvents[145] = {5,13,17,20,21,22,26,27,40,42,43,45,50,54,58,66,67,70,74,78,79,81,82,89,101,107,108,110,114,115,116,121,132,136,140,143,151,153,155,156,159,160,167,171,173,177,182,192,193,197,199,202,205,216,230,231,233,234,236,239,241,243,252,267,269,270,274,279,280,283,284,286,295,299,300,302,306,307,309,318,320,328,333,335,337,338,350,357,360,369,370,374,376,377,378,384,386,387,391,403,407,410,413,418,420,427,428,432,443,444,449,454,460,464,466,467,471,475,478,480,481,482,485,487,492,493,496,502,514,519,523,526,532,541,544,549,551,554,555,557,561,563,565,576,577};
array <int> selPic[36] = {1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65,69,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72};