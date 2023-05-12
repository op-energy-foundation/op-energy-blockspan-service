import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BaseBoxHorComponent } from './base-box-hor.component';

describe('BaseBoxHorComponent', () => {
  let component: BaseBoxHorComponent;
  let fixture: ComponentFixture<BaseBoxHorComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [BaseBoxHorComponent],
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BaseBoxHorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
