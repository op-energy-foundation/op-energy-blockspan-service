import { TableColumn } from '../../interfaces/oe-energy.interface';

export const TABLE_HEADERS: TableColumn[] = [
  { displayName: 'Block', columnKey: 'block' },
  { displayName: 'Median Time', columnKey: 'strikeMediantime' },
  {
    displayName: 'Observed Block Mediantime',
    columnKey: 'observedBlockMediantime',
    defaultValue: '?',
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
