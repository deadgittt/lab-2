# Лабораторная работа №2
**Выполнил**: Гайдеров Ярослав Игоревич \
**Группа**: Р3431 \
**Преподаватель**: Пенской Александр Владимирович \
**Язык**: Haskell
___
## Вариант: bt-bag
**Цель:**  
Освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing), а также разделением интерфейса и особенностей реализации.

**Формулировка:**  
Реализовать неизменяемую структуру данных `Binary Tree`, используя интерфейс `Bag(multiset)`

**Требования**
1. Функции:
   1. - [x] добавление и удаление элементов;
   2. - [x] фильтрация;
   3. - [x] отображение (map);
   4. - [x] свертки (левая и правая);
   5. - [x] структура должна быть моноидом.
2. - [x] Unit-тесты
3. - [x] Property-based тесты (как минимум 3 свойства, включая свойства моноида)
4. - [x] Структура должна быть полиморфной

---

## Структура данных: bt-bag

**Файл:** `src/Bag.hs` (библиотека), `src/Tree.hs` (базовое бинарное дерево)

**Ключевые элементы реализации:**
- `Tree`: `Empty | Node left value right`, симметричный обход `toList`, кастомный `Show`;
- `Bag`: обертка `newtype Bag a = Bag (Tree (Entry a))`, где `Entry a = Entry a Int` хранит значение и кратность;
- API: `empty`, `singleton`, `insert/insertCount`, `deleteOne/deleteAll`, `member`, `count`, `size`, `distinctSize`, `toList/fromList`, `mapBag`, `filterBag`, `foldrBag`, `foldlBag`
- Моноид: `Semigroup/Monoid` через объединение bag'ов с суммированием кратностей;
- `Eq` на уровне мультимножества (сравнение обходов);

**Тесты:** `test/Spec.hs`
- Unit: проверки для `Bag Int` и `Bag Char` (size/distinctSize, count, member, deleteOne/deleteAll, toList).
- Property-based (QuickCheck): корректность insert/delete, инварианты size/distinctSize, roundtrip toList/fromList, filter/map соответствуют списковым операциям, моноидные законы и сумма кратностей при `(<>)`.

**Линтер и форматтер:** HLint + Fourmolu
