import { ComponentFixture, TestBed } from '@angular/core/testing';

import { BlockrateSummaryV2Component } from './blockrate-summary-v2.component';

describe('BlockrateSummaryV2Component', () => {
  let component: BlockrateSummaryV2Component;
  let fixture: ComponentFixture<BlockrateSummaryV2Component>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ BlockrateSummaryV2Component ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockrateSummaryV2Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
