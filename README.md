echo_cluster
============
Описание задачи:
https://www.evernote.com/shard/s380/sh/71ea3cd1-ad65-44f9-a8b2-78754c26010e/c33132299ce0801fdc9f2e2299b80541

Информация о проекте
---------------------
-   Язык: Erlang
-   Основа: FSM
-   Сетевой протокол: UDP
-   Структура:
    -   apps: поставляемые приложения
        -   common: утилиты и общие headers
        -   echo_cluster: главное приложение
    -   deps: зависимости
    -   rel: релизы
-   Сборка: make и rebar

Данный проект написан используя TDD. Основные тестовые сценарии находятся в модуле distributed_tests.
Первым этапом тестирования является запуск тестов (см. Run Tests ниже).
Затем можно провести уже ручное тестирование. Запустив 7 терминалов, в каждом из них
необходимо запустить одну из нод (см. Run ниже).
После этого, в консоли можно посмотреть, кто сейчас мастер:
    cluster_gate_fsm:master(self()). затем flush().
остановить или запустить приложение:
    application:stop(echo_cluster).
    application:start(echo_cluster).

Что нужно, чтобы собрать и запустить проект
---------------------
-   debian 7 или ubuntu >12.04 (код и релизы не тестировались под BSD, Mac и Win)
-   make,
-   git,
-   erlang R16 - можно собрать из исходников или добыть тут:
                 https://www.erlang-solutions.com/downloads/download-erlang-otp
-   rebar - предполагается, что rebar уже установлен и у вас получилось что-то похожее после вызова `which rebar`
            `/usr/bin/rebar`

Build
---------------------
    $ tar xvfz echo_cluster.tar.gz
    $ cd echo_cluster
    $ make

Run Tests
---------------------
    $ make test

Release
---------------------
    $ make rel

Run
---------------------
    В данном примере уже сгенерированно 7 узлов. Они находятся в папке `rel`.
    Например для запуска узла 5 в отладочном режиме необходимо в каталоге проекта выполнить
    $ rel/node5/echo_cluster/bin/node5 console

Build notice
---------------------
    covertool тянет за собой rebar в зависимостях, из-за этого появляются .so файлы в priv.

Что Вы должны увидеть после запуска make test
---------------------
```
λ ~/projects/echo_cluster/ master* make test
...
==> echo_cluster (eunit)
Compiled test/ec_test_tools.erl
Compiled test/common_tests.erl
Compiled test/simple_distributed_tests.erl
Compiled src/echo_cluster_app.erl
Compiled test/start_stop_tests.erl
Compiled src/echo_cluster_sup.erl
Compiled test/distributed_tests.erl
Compiled src/cluster_gate_fsm.erl
======================== EUnit ========================
module 'ec_test_tools'
module 'cluster_gate_fsm'
module 'echo_cluster_sup'
module 'echo_cluster_app'
module 'common_tests'
  common_tests: config_read_test_...ok
  [done in 0.003 s]
module 'start_stop_tests'
  start_stop_tests: main_test_...[0.081 s] ok
  [done in 0.083 s]
module 'simple_distributed_tests'
  simple_distributed_tests: stop_master_test_...[4.475 s] ok
  simple_distributed_tests: shuffle_stop_test_...[4.462 s] ok
  [done in 10.973 s]
module 'distributed_tests'
  distributed_tests: order_start_test_ (start cluster in order)...[5.274 s] ok
  distributed_tests: shuffle_start_test_ (shuffle nodes and try to start this)...[5.274 s] ok
  distributed_tests: slave_halt_test_ (halt slave and check than master not changed)...[6.979 s] ok
  distributed_tests: complex_case_test_ (complex test: halt 7 than 5 and 6 after it start 7)...[13.391 s] ok
  [done in 34.994 s]
=======================================================
  All 8 tests passed.
...
```
