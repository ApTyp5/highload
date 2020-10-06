Веб-сервер, утилизирующий все возможные ядра
=====

Запуск
-----
Для запуска понадобится утилита [rebar3](https://github.com/erlang/rebar3).

    ./start

Apache Benchmark
----
```
$ ab -c 10 -n 10000 http://localhost:80/httptest/wikipedia_russia.html
This is ApacheBench, Version 2.3 <$Revision: 1843412 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking localhost (be patient)
Completed 1000 requests
Completed 2000 requests
Completed 3000 requests
Completed 4000 requests
Completed 5000 requests
Completed 6000 requests
Completed 7000 requests
Completed 8000 requests
Completed 9000 requests
Completed 10000 requests
Finished 10000 requests


Server Software:        rlang
Server Hostname:        localhost
Server Port:            80

Document Path:          /httptest/wikipedia_russia.html
Document Length:        954824 bytes

Concurrency Level:      10
Time taken for tests:   4.803 seconds
Complete requests:      10000
Failed requests:        0
Total transferred:      9549120000 bytes
HTML transferred:       9548240000 bytes
Requests per second:    2081.84 [#/sec] (mean)
Time per request:       4.803 [ms] (mean)
Time per request:       0.480 [ms] (mean, across all concurrent requests)
Transfer rate:          1941385.35 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    2  40.9      0    1033
Processing:     1    3   1.5      3      24
Waiting:        0    1   1.0      1      12
Total:          1    5  40.9      3    1035

Percentage of the requests served within a certain time (ms)
  50%      3
  66%      3
  75%      3
  80%      4
  90%      4
  95%      6
  98%      8
  99%      9
 100%   1035 (longest request)
```

Apache Benchmark of nginx
----
```
ab -c 10 -n 10000 http://localhost:80/httptest/wikipedia_russia.html
This is ApacheBench, Version 2.3 <$Revision: 1843412 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking localhost (be patient)
Completed 1000 requests
Completed 2000 requests
Completed 3000 requests
Completed 4000 requests
Completed 5000 requests
Completed 6000 requests
Completed 7000 requests
Completed 8000 requests
Completed 9000 requests
Completed 10000 requests
Finished 10000 requests


Server Software:        nginx/1.16.1
Server Hostname:        localhost
Server Port:            80

Document Path:          /httptest/wikipedia_russia.html
Document Length:        954824 bytes

Concurrency Level:      10
Time taken for tests:   3.404 seconds
Complete requests:      10000
Failed requests:        0
Total transferred:      9550710000 bytes
HTML transferred:       9548240000 bytes
Requests per second:    2938.04 [#/sec] (mean)
Time per request:       3.404 [ms] (mean)
Time per request:       0.340 [ms] (mean, across all concurrent requests)
Transfer rate:          2740268.41 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.0      0       1
Processing:     1    3   0.5      3      17
Waiting:        0    0   0.1      0       4
Total:          1    3   0.6      3      17

Percentage of the requests served within a certain time (ms)
  50%      3
  66%      3
  75%      3
  80%      4
  90%      4
  95%      4
  98%      5
  99%      5
 100%     17 (longest request)
```