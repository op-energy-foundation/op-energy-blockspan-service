import { TableColumn } from '../../interfaces/oe-energy.interface';

export const TABLE_HEADERS: TableColumn[] = [
  { displayName: 'Block', columnKey: 'block', isClickable: true },
  { displayName: 'Median Time', columnKey: 'strikeMediantime', isClickable: true },
  {
    displayName: 'Observed Block Mediantime',
    columnKey: 'observedBlockMediantime',
    defaultValue: '?',
    isClickable: true,
  },
  {
    displayName: 'Observed Result',
    columnKey: 'observedResult',
    defaultValue: '?',
  },
  {
    displayName: 'Total Number Of Guess',
    columnKey: 'guessesCount',
    defaultValue: '0',
  },
];
