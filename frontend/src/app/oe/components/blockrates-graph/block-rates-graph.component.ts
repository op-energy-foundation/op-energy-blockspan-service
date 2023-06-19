import { Component, OnInit } from '@angular/core';
import { EChartsOption, graphic } from 'echarts';
import { Observable, Subscription } from 'rxjs';
import { ActivatedRoute } from '@angular/router';
import { ToastrService } from 'ngx-toastr';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { BlockSpanHeadersNbdr } from '../../interfaces/oe-energy.interface';
import { downloadChart } from '../../utils/helper';
import { NgbCalendar, NgbDate, NgbDateParserFormatter } from '@ng-bootstrap/ng-bootstrap';

@Component({
  selector: 'app-block-rates-graph',
  templateUrl: './block-rates-graph.component.html',
  styleUrls: ['./block-rates-graph.component.scss'],
  styles: [
    `
      .loadingGraphs {
        position: absolute;
        top: 50%;
        left: calc(50% - 15px);
        z-index: 100;
      }
    `,
  ],
})
export class BlockRatesGraphComponent implements OnInit {
  hoveredDate: NgbDate | null = null;
  fromDate: NgbDate | null;
  toDate: NgbDate | null;
  minDate: NgbDate | null;
  maxDate: NgbDate | null;
  blocksList: BlockSpanHeadersNbdr[] = [];
  chartOptions: EChartsOption = {};
  chartInitOptions = {
    renderer: 'svg',
  };
  blockSpan: number = 144;
  statsObservable$: Observable<any>;
  isLoading = true;
  subscription: Subscription;
  chartInstance: any = undefined;

  constructor(
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private toastr: ToastrService,
    private calendar: NgbCalendar,
    public formatter: NgbDateParserFormatter
  ) {
    this.resetDateFilter();
  }

  resetDateFilter() {
    const current = this.calendar.getToday();

    // Set the minimum date to January 11, 2009
    this.minDate = new NgbDate(2009, 1, 11);

    // Set the maximum date to the current date
    this.maxDate = new NgbDate(current.year, current.month, current.day);

    // Set the starting date to January 11, 2009
    this.fromDate = new NgbDate(2009, 1, 11);
    this.toDate = this.calendar.getToday();
  }

  ngOnInit(): void {
    this.route.paramMap.subscribe((params) => {
      this.blockSpan = parseInt(params.get('span'), 10);
      this.resetDateFilter();
      this.fetchAndRenderChart();
    });
  }

  fetchAndRenderChart(): void {
    this.oeEnergyApiService
    .$getBlocksWithNbdrByBlockSpan(0, this.blockSpan)
    .subscribe(
      (blocks: any[]) => {
        this.prepareChartOptions(blocks);
        this.isLoading = false;
        this.blocksList = blocks;
      },
      (error) => {
        this.isLoading = false;
        this.toastr.error('Blockspans are not found!', 'Failed!');
      }
    );
  }

  prepareChartOptions(data: BlockSpanHeadersNbdr[]): void {
    let title: object;
    const chartData = data.map((item) => ({
      nbdr: item.nbdr,
      label: `${item.startBlock.height}-${item.endBlock.height} (${new Date(
        item.endBlock.timestamp * 1000
      ).toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'long',
        day: 'numeric',
      })})`,
    }));

    // Calculate the min and max values of nbdr with 10% buffer
    const minValue = Math.min(...chartData.map(item => item.nbdr));
    const maxValue = Math.max(...chartData.map(item => item.nbdr));
    const yAxisMin = (Math.floor(minValue*0.9));
    const yAxisMax = this.roundToNearest(Math.ceil(maxValue*1.1));
    this.chartOptions = {
      title: title,
      color: [
        new graphic.LinearGradient(0, 0, 0, 0.65, [
          { offset: 0, color: '#F4511E99' },
          { offset: 0.25, color: '#FB8C0099' },
          { offset: 0.5, color: '#FFB30099' },
          { offset: 0.75, color: '#FDD83599' },
          { offset: 1, color: '#7CB34299' },
        ]),
        '#D81B60',
        new graphic.LinearGradient(0, 0, 0, 0.65, [
          { offset: 0, color: '#F4511E' },
          { offset: 0.25, color: '#FB8C00' },
          { offset: 0.5, color: '#FFB300' },
          { offset: 0.75, color: '#FDD835' },
          { offset: 1, color: '#7CB342' },
        ]),
      ],
      animation: false,
      grid: {
        top: 30,
        bottom: 80,
        left: 30,
        right: 0,
      },
      tooltip: {
        show: !this.isMobile(),
        trigger: 'axis',
        axisPointer: {
          type: 'line',
        },
        backgroundColor: 'rgba(17, 19, 31, 1)',
        borderRadius: 4,
        shadowColor: 'rgba(0, 0, 0, 0.5)',
        textStyle: {
          color: '#b1b1b1',
          align: 'left',
        },
        borderColor: '#000',
        formatter: (data): string => {
          let tooltip = `<b style="color: white; margin-left: 2px;">
            ${chartData[data[0].dataIndex].label}</b><br>`;

          tooltip += `${data[0].marker} ${data[0].seriesName}: ${data[0].data}<br>`;
          return tooltip;
        },
      },
      xAxis: {
        type: 'category',
        data: chartData.map((item) => item.nbdr),
        axisLabel: {
          formatter: (_value, index) => {
            return chartData[index].label;
          },
          padding: [0, -100, 0, 0],
        }
      },
      yAxis: {
        type: 'value',
        min: yAxisMin,
        max: yAxisMax,
        splitLine: {
          lineStyle: {
            type: 'dotted',
            color: '#ffffff66',
            opacity: 0.25,
          },
        },
      },
      series: [
        {
          legendHoverLink: false,
          zlevel: 0,
          yAxisIndex: 0,
          name: 'Nbdr',
          data: chartData.map((item) => item.nbdr),
          type: 'line',
          smooth: 0.25,
          symbol: 'none',
          lineStyle: {
            width: 1.5,
            opacity: 1,
          },
        },
      ],
      dataZoom: [
        {
          type: 'inside',
          realtime: true,
          zoomLock: true,
          maxSpan: 100,
          minSpan: 5,
          moveOnMouseMove: false,
        },
        {
          showDetail: false,
          show: true,
          type: 'slider',
          brushSelect: false,
          realtime: true,
          left: 10,
          right: 0,
          selectedDataBackground: {
            lineStyle: {
              color: '#fff',
              opacity: 0.45,
            },
            areaStyle: {
              opacity: 0,
            },
          },
        },
      ],
    };
  }

  onChartInit(ec) {
    this.chartInstance = ec;
  }

  isMobile() {
    return window.innerWidth <= 767.98;
  }

  onSaveChart() {
    // @ts-ignore
    const prevBottom = this.chartOptions.grid.bottom;
    const now = new Date();
    // @ts-ignore
    this.chartOptions.grid.bottom = 40;
    this.chartOptions.backgroundColor = '#11131f';
    this.chartInstance.setOption(this.chartOptions);
    downloadChart(
      this.chartInstance.getDataURL({
        pixelRatio: 2,
        excludeComponents: ['dataZoom'],
      }),
      `block-rates-${this.blockSpan}-${Math.round(now.getTime() / 1000)}.svg`
    );
    // @ts-ignore
    this.chartOptions.grid.bottom = prevBottom;
    this.chartOptions.backgroundColor = 'none';
    this.chartInstance.setOption(this.chartOptions);
  }

  onDateSelection(date: NgbDate) {
    if (!this.fromDate && !this.toDate) {
      this.fromDate = date;
    } else if (
      this.fromDate &&
      !this.toDate &&
      date &&
      date.after(this.fromDate)
    ) {
      this.toDate = date;
    } else {
      this.toDate = null;
      this.fromDate = date;
    }
    
    if (this.fromDate && this.toDate) {
      this.prepareChartOptions(
        this.blocksList.filter((obj) => {
          const timestamp = obj.endBlock.timestamp * 1000; // Convert seconds to milliseconds
          const date = new Date(timestamp);
          return (
            date >=
              new Date(
                this.fromDate.year,
                this.fromDate.month - 1,
                this.fromDate.day
              ) &&
            date <=
              new Date(this.toDate.year, this.toDate.month - 1, this.toDate.day)
          );
        })
      );
    }
  }

  isHovered(date: NgbDate) {
    return (
      this.fromDate &&
      !this.toDate &&
      this.hoveredDate &&
      date.after(this.fromDate) &&
      date.before(this.hoveredDate)
    );
  }

  isInside(date: NgbDate) {
    return this.toDate && date.after(this.fromDate) && date.before(this.toDate);
  }

  isRange(date: NgbDate) {
    return (
      date.equals(this.fromDate) ||
      (this.toDate && date.equals(this.toDate)) ||
      this.isInside(date) ||
      this.isHovered(date)
    );
  }

  validateInput(currentValue: NgbDate | null, input: string): NgbDate | null {
    const parsed = this.formatter.parse(input);
    return parsed && this.calendar.isValid(NgbDate.from(parsed))
      ? NgbDate.from(parsed)
      : currentValue;
  }

  setStartDate(date: NgbDate, datePicker: any) {
    datePicker.startDate = {...date};
    datePicker.toggle();
  }

  roundToNearest = (value: number, roundTo: number = 50) => {
    return Math.round(value / roundTo) * roundTo;
  };
  
}
