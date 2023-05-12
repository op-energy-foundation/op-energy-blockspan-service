import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BaseBoxV2Component } from './base-box-v2.component';

describe('BaseBoxV2Component', () => {
  let component: BaseBoxV2Component;
  let fixture: ComponentFixture<BaseBoxV2Component>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [BaseBoxV2Component],
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BaseBoxV2Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
