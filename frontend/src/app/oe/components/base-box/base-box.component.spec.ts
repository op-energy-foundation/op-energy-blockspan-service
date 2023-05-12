import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BaseBoxComponent } from './base-box.component';

describe('BaseBoxComponent', () => {
  let component: BaseBoxComponent;
  let fixture: ComponentFixture<BaseBoxComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [BaseBoxComponent],
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BaseBoxComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
