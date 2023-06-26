import { Component, OnInit } from '@angular/core';
import { EChartsOption, graphic } from 'echarts';
import { Observable, Subscription, switchMap } from 'rxjs';
import { ActivatedRoute, Router } from '@angular/router';
import { ToastrService } from 'ngx-toastr';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { BlockSpanHeadersNbdr } from '../../interfaces/oe-energy.interface';
import { downloadChart } from '../../utils/helper';
import {
  NgbCalendar,
  NgbDate,
  NgbDateParserFormatter,
} from '@ng-bootstrap/ng-bootstrap';
import { Location } from '@angular/common';

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
  showLessDataPoints = false;
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
  supportedCharts = [6, 144, 1008, 2016];
  limitedDataCharts = [6, 144, 1008];
  blockSpan: number = 144;
  statsObservable$: Observable<any>;
  isLoading = true;
  subscription: Subscription;
  chartInstance: any = undefined;
  paramsSubs: Subscription;
  querySubs: Subscription;
  chartData = [] as {
    startBlockHeight: number;
    endBlockHeight: number;
    label: string;
    nbdr: number;
  }[];

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private location: Location,
    private oeEnergyApiService: OeEnergyApiService,
    private toastr: ToastrService,
    private calendar: NgbCalendar,
    public formatter: NgbDateParserFormatter
  ) {}

  ngOnDestroy(): void {
    if (this.querySubs) {
      this.querySubs.unsubscribe();
    }

    if (this.paramsSubs) {
      this.paramsSubs.unsubscribe();
    }
  }

  resetDateFilter(startDate?: Date, endDate?: Date) {
    const current = this.calendar.getToday();

    // Set the minimum date to January 11, 2009
    this.minDate = new NgbDate(2009, 1, 1);

    // Set the maximum date to the current date
    this.maxDate = new NgbDate(current.year, current.month, current.day);

    // Set the starting date to January 11, 2009
    this.fromDate = startDate
      ? new NgbDate(
          startDate.getFullYear(),
          startDate.getMonth() + 1,
          startDate.getDate()
        )
      : new NgbDate(2009, 1, 11);
    this.toDate = endDate
      ? new NgbDate(
          endDate.getFullYear(),
          endDate.getMonth() + 1,
          endDate.getDate()
        )
      : null;

    this.onDateChange(
      this.convertToJSDate(this.fromDate),
      this.convertToJSDate(this.toDate)
    );
  }

  ngOnInit(): void {
    this.route.paramMap
      .pipe(
        switchMap((params) => {
          this.isLoading = true;
          this.blocksList = [];
          this.blockSpan = parseInt(params.get('span'), 10);

          if (!this.supportedCharts.includes(this.blockSpan)) {
            this.toastr.error('Chart not supported', 'Failed!');
            this.router.navigate(['/']);
            return;
          }
          if (this.limitedDataCharts.includes(this.blockSpan)) {
            this.showLessDataPoints = true;
          }
          return this.route.queryParamMap;
        })
      )
      .subscribe((queryParams) => {
        const startDateParam = queryParams.get('startDate');
        const endDateParam = queryParams.get('endDate');
        const startDate = startDateParam ? new Date(startDateParam) : null;
        const endDate = endDateParam ? new Date(endDateParam) : new Date();
        if (startDateParam) {
          this.showLessDataPoints = false;
        }
        this.fetchAndRenderChart(startDate, endDate);
      });
  }

  fetchAndRenderChart(startDate: Date, endDate: Date): void {
    this.oeEnergyApiService
      .$getBlocksWithNbdrByBlockSpan(0, this.blockSpan)
      .subscribe(
        (blocks: any[]) => {
          if (this.showLessDataPoints) {
            const data = blocks.slice(-512);
            if (data.length > 0) {
              this.resetDateFilter(
                new Date(data[0].endBlock.timestamp * 1000),
                new Date()
              );
            }
          } else {
            this.resetDateFilter(startDate, endDate);
          }
          this.prepareChartOptions(blocks);
          this.isLoading = false;
          this.blocksList = blocks;
        },
        (error) => {
          this.isLoading = false;
          this.blocksList = [];
          this.prepareChartOptions([]);
          this.toastr.error('Blockspans are not found!', 'Failed!');
        }
      );
  }

  prepareChartOptions(data: BlockSpanHeadersNbdr[]): void {
    data = data.filter((obj) => {
      const timestamp = obj.endBlock.timestamp * 1000; // Convert seconds to milliseconds
      const date = new Date(timestamp);
      const fromDate = new Date(
        this.fromDate.year,
        this.fromDate.month - 1,
        this.fromDate.day
      );
      if (this.toDate) {
        const toDate = new Date(
          this.toDate.year,
          this.toDate.month - 1,
          this.toDate.day
        );
        return date >= fromDate && date <= toDate;
      } else {
        return date >= fromDate;
      }
    });

    let title: object;
    const chartData = data.map((item) => ({
      nbdr: item.nbdr,
      startBlockHeight: item.startBlock.height,
      endBlockHeight: item.endBlock.height,
      label: `${item.startBlock.height}-${item.endBlock.height} (${new Date(
        item.endBlock.timestamp * 1000
      ).toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'long',
        day: 'numeric',
      })})`,
    }));

    // Calculate the min and max values of nbdr with 10% buffer
    const minValue = Math.min(...chartData.map((item) => item.nbdr));
    const maxValue = Math.max(...chartData.map((item) => item.nbdr));
    const yAxisMin = Math.floor(minValue * 0.9);
    const yAxisMax = Math.ceil(maxValue * 1.1);
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
        },
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
          symbol: 'triangle',
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

    this.chartData = chartData;
  }

  handleChartClick(params) {
    if (
      params.componentSubType === 'line' &&
      params.componentType === 'series'
    ) {
      // Handle click event for line or bar series
      const dataPoint = this.chartData[params.dataIndex];
      // redirecting user to energy details page
      window.open(
        `${document.location.protocol}//${document.location.host}/hashstrikes/energy_detail/${dataPoint.startBlockHeight}/${dataPoint.endBlockHeight}`,
        '_blank'
      );
    }
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

  convertToJSDate(ngbDate: NgbDate): string {
    if (!ngbDate) return null;

    const year = ngbDate.year.toString();
    const month = ngbDate.month.toString().padStart(2, '0');
    const day = ngbDate.day.toString().padStart(2, '0');

    return `${year}-${month}-${day}`;
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
    this.onDateChange(
      this.convertToJSDate(this.fromDate),
      this.convertToJSDate(this.toDate)
    );
  }

  onDateChange(startDate: string, endDate: string): void {
    const queryParams = {};
    queryParams['startDate'] = startDate;
    queryParams['endDate'] = endDate;

    this.prepareChartOptions(this.blocksList);
    const urlTree = this.router.createUrlTree([], { queryParams });
    const url = this.router.serializeUrl(urlTree);
    this.location.replaceState(url);
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
    datePicker.startDate = { ...date };
    datePicker.toggle();
  }
}
