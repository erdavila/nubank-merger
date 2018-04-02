#!/usr/bin/env python3
import fileinput

ASSUMED_YEAR = 2018

MONTHS = dict(
    (month, num)
    for num, month in enumerate(
        [
            'JAN', 'FEV', 'MAR', 'ABR', 'MAI', 'JUN',
            'JUL', 'AGO', 'SET', 'OUT', 'NOV', 'DEZ',
        ],
        1
    )
)


def main():
    print("date,category,title,amount")
    for (val1, val2) in grouped(fileinput.input(), 2):
        date = reformat_date(val1.strip())
        title, amount = val2.strip().split('\t')
        amount = amount.replace('.', '').replace(',', '.')

        if title == 'Pagamento recebido':
            category = 'Pagamento'
        else:
            category = ''

        print(date, category, title, amount, sep=',')


def grouped(items, count):
    while items:
        grouped_items = []
        for _ in range(count):
            item = next(items)
            grouped_items.append(item)
        yield grouped_items


def reformat_date(date):
    day, month = date.split(' ')
    return '%04d-%02d-%02d' % (ASSUMED_YEAR, MONTHS[month], int(day))


if __name__ == '__main__':
    main()
