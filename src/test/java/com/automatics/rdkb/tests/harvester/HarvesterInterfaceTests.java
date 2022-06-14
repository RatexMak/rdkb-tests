/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.harvester;

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;

public class HarvesterInterfaceTests extends AutomaticsTestBase {

	/**
	 * Validate Wi-Fi Harvester InterfaceDevicesWifi Report after Reboot of DUT
	 * <ol>
	 * <li>Verification of InterfaceDevicesWifi Reporting Period using the TR181
	 * parameter-Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.ReportingPeriod
	 * to get the InterfaceDevicesWifi Reporting Period value before reset the
	 * device</li>
	 * <li>Verification of InterfaceDevicesWifi Polling Period using the TR181
	 * parameter-Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.PollingPeriod
	 * to get the InterfaceDevicesWifi Polling Period value before reset the
	 * device</li>
	 * <li>Verification of RadioInterfaceStatistics Reporting Period using the TR181
	 * parameter-RadioInterfaceStatistics.ReportingPeriod to get the
	 * RadioInterfaceStatistics Reporting Period value before reset the device</li>
	 * </ol>
	 *
	 * @param device
	 * @author RAJAPANDIAN
	 * @refactor Athira
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-VALIDATE-HARV-3001")
	public void validateHarvParamAfterReset(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-VALIDATE-HARV-301";
		String stepNum = "s1";
		String errorMessage = null;
		boolean status = false;
		String reportingPeriodValue = null;
		// Variable Declaration Ends
		// Steps related to Factory reset and default value validation has been moved to
		// TC-RDKB-DEFAULT-VALUES-1001
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-VALIDATE-HARV-3001");
		LOGGER.info("TEST DESCRIPTION: Validate Wi-Fi Harvester InterfaceDevicesWifi Report after Reboot of DUT");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Verification of InterfaceDevicesWifi Reporting Period using the TR181 parameter-Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.ReportingPeriod  to get the  InterfaceDevicesWifi Reporting Period value before reset the device");
		LOGGER.info(
				"2. Verification of InterfaceDevicesWifi Polling Period using the TR181 parameter-Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.PollingPeriod  to get the  InterfaceDevicesWifi Polling Period value before reset the device");
		LOGGER.info(
				"3. Verification of RadioInterfaceStatistics Reporting Period using the TR181 parameter-RadioInterfaceStatistics.ReportingPeriod  to get the  RadioInterfaceStatistics Reporting Period  value before reset the device");
		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "Unable to get InterfaceDevicesWifi.ReportingPeriod value";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Verification of InterfaceDevicesWifi Reporting Period using the TR181 parameter-Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.ReportingPeriod  to get the  InterfaceDevicesWifi Reporting Period value before reset the device");
			LOGGER.info(
					"STEP 1: ACTION : Execute the Webpa get command on  Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.ReportingPeriod  to get the  InterfaceDevicesWifi Reporting Period value  ");
			LOGGER.info(
					"STEP 1: EXPECTED : InterfaceDevicesWifi.ReportingPeriod status should have the default value as 900 before reset the device");
			LOGGER.info("**********************************************************************************");

			reportingPeriodValue = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD);
			LOGGER.info(
					"INTERFACE DEVICE REPORTING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : " + reportingPeriodValue);
			if (CommonUtils.isNotEmptyOrNull(reportingPeriodValue)) {
				status = BroadBandTestConstants.APPLICABLE_VALUES_FOR_HARVESTER_DEVICE_WIFI_PARAMETERS
						.contains(reportingPeriodValue);
			}
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : Successfully get the InterfaceDevicesWifi Reporting Period default value as 900 before reset the device");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s2";
			errorMessage = "Unable to get InterfaceDevicesWifi.PollingPeriod value";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verification of InterfaceDevicesWifi Polling Period using the TR181 parameter-Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.PollingPeriod  to get the  InterfaceDevicesWifi Polling Period value before reset the device");
			LOGGER.info(
					"STEP 2: ACTION : Execute the Webpa get command on  Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.PollingPeriod    ");
			LOGGER.info(
					"STEP 2: EXPECTED : InterfaceDevicesWifi.PollingPeriod should have  the  default value as 900 before reset the device.");
			LOGGER.info("**********************************************************************************");
			reportingPeriodValue = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD);
			LOGGER.info("INTERFACE DEVICE POLLING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : " + reportingPeriodValue);
			if (CommonUtils.isNotEmptyOrNull(reportingPeriodValue)) {
				status = BroadBandTestConstants.APPLICABLE_VALUES_FOR_HARVESTER_DEVICE_WIFI_PARAMETERS
						.contains(reportingPeriodValue);
			}
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : Successfully get the InterfaceDevicesWifi Polling Period default value as 900 before reset the device");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s3";
			errorMessage = "Unable to get  RadioInterfaceStatistics Reporting Period  value.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Verification of RadioInterfaceStatistics Reporting Period using the TR181 parameter-RadioInterfaceStatistics.ReportingPeriod  to get the  RadioInterfaceStatistics Reporting Period  value before reset the device");
			LOGGER.info(
					"STEP 3: ACTION : Execute the Webpa get command on  Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.ReportingPeriod  ");
			LOGGER.info(
					"STEP 3: EXPECTED :  RadioInterfaceStatistics Reporting Period  should have  the  default value as 900 before reset the device");
			LOGGER.info("**********************************************************************************");

			reportingPeriodValue = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD);
			LOGGER.info("RADIO INTERFACE REPORTING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : " + reportingPeriodValue);
			if (CommonUtils.isNotEmptyOrNull(reportingPeriodValue)) {
				status = BroadBandTestConstants.APPLICABLE_VALUES_FOR_HARVESTER_DEVICE_WIFI_PARAMETERS
						.contains(reportingPeriodValue);
			}
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Successfully get the RadioInterfaceStatistics Reporting Period default value as 900 before reset the device");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (

		Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * POST-CONDITION 1 : ENABLE THE PUBLIC WIFI
			 */
			BroadBandPostConditionUtils.executePostConditionToEnableOrDisablePublicWifiBasedOnStbProperty(device,
					tapEnv, BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-VALIDATE-HARV-3001");
	}

	/**
	 * Validate Enable-Disable status configuration of Interface Devices Wifi
	 * Report, Radio Interface Statistics Report values after reboot
	 * <ol>
	 * <li>Verify Getting the Default Value for the WEBPA parameter:
	 * Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled and Enable
	 * InterfaceDevicesWifi harvester report by setting the value as true via WEBPA
	 * parameter Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled</li>
	 * <li>Verify Getting the Default Value for the WEBPA parameter:
	 * Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.ReportingPeriod And
	 * Verify setting the ReportingPeriod value to 300 using the WEBPA parameter
	 * Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.ReportingPeriod</li>
	 * <li>Verify Getting the Default Value for the WEBPA
	 * parameter:Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.PollingPeriod
	 * And Verify setting the PollingPeriod value to 300 using the WEBPA parameter
	 * Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.PollingPeriod</li>
	 * <li>Verify Getting the Default Value for the WEBPA parameter:
	 * Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.Enabled And Verify
	 * setting the RadioInterfaceStatistics.Enabled value to true using the WEBPA
	 * parameter
	 * Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.Enabled</li>
	 * <li>Verify Getting the Default Value for the WEBPA parameter:
	 * Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.PollingPeriod And
	 * Verify setting the .PollingPeriod value to 60 using the WEBPA parameter
	 * Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.PollingPeriod</li>
	 * <li>Verify Getting the Default Value for the WEBPA parameter:
	 * Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.ReportingPeriod And
	 * Verify setting the ReportingPeriod value to 60 using the WEBPA parameter
	 * Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.ReportingPeriod</li>
	 * <li>Perform reboot On device</li>
	 * <li>Verify whether WebPA is Up and Running in the Device after reboot</li>
	 * <li>Verify the value is retained for
	 * Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled after performing
	 * Reboot .</li>
	 * <li>Verify the value is reverted to the default value 900 for
	 * Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.ReportingPeriod after
	 * performing Reboot .</li>
	 * <li>Verify the value is reverted to the default value 900 for
	 * Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.PollingPeriod after
	 * performing Reboot .</li>
	 * <li>Verify the value is retained for
	 * Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.Enabled after
	 * performing Reboot .</li>
	 * <li>Verify the value is reverted to the default value 900 for
	 * Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.PollingPeriod after
	 * performing Reboot .</li>
	 * <li>Verify the value is reverted to the default value 900 for
	 * Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.ReportingPeriod after
	 * performing Reboot .</li>
	 * <li>POST-CONDITION 1 : Verify Reverting the default values by setting the
	 * InterfaceDevicesEnable, InterfaceDevice ReportingPeriod,InterfaceDevice
	 * PollingPeriod,
	 * RadioInterfaceStatisticsEnable,RadioInterfaceStatistics.ReportingPeriod,RadioInterfaceStatistics.PollingPeriod
	 * value to Default Values Retrieved using the WEBPA</li>
	 * </ol>
	 *
	 * @param device {@link Dut}
	 * @author Vignesh
	 * 
	 * @Refactor Sruthi Santhosh
	 * 
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-HARV-6009")
	public void testToVerifyDefaultValuesForRadioInterfaceAndInterfaceDeviceParameterPostReboot(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-HARV-609";
		String stepNum = "s1";
		String errorMessage = "";
		boolean status = false;
		String interfaceDeviceWifiEnabledValue = null;
		String reportingPeriodValue = null;
		String pollingPeriodValue = null;
		String radioInterfaceStatisticsEnabledValue = null;
		String interfaceStatisticsReportingPeriodValue = null;
		String interfaceStatisticsPollingPeriodValue = null;
		List<WebPaParameter> webPaParameters = new ArrayList<>();
		BroadBandResultObject resultObject = null;
		String interfaceDeviceEnableStatusPostReboot = null;
		String interfaceDeviceReportingPeriodPostReboot = null;
		String interfaceDevicePollingPeriodAfterReboot = null;
		String interfaceStatisticsEnableStatusAfterReboot = null;
		String interfaceStatisticsPollingPeriodPostReboot = null;
		String interfaceStatisticsReportingPeriodPostReboot = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-HARV-6009");
		LOGGER.info(
				"TEST DESCRIPTION: Validate Enable-Disable status configuration of Interface Devices Wifi Report, Radio Interface Statistics Report values after reboot");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify Getting the Default Value for the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT
				+ " and  Enable InterfaceDevicesWifi harvester "
				+ "report by setting the value as true via WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
		LOGGER.info("2. Verify Getting the Default Value for the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + " and Verify setting the "
				+ "ReportingPeriod value to 300 using the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD);
		LOGGER.info("3. Verify Getting the Default Value for the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + " and Verify setting the "
				+ "PollingPeriod value to 300 using the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD);
		LOGGER.info("4. Verify Getting the Default Value for the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS + " and Verify setting the "
				+ "RadioInterfaceStatistics.Enabled value to true using the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
		LOGGER.info("5. Verify Getting the Default Value for the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD + " and Verify setting the"
				+ "PollingPeriod value to 60 using the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD);
		LOGGER.info("6. Verify Getting the Default Value for the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD + " and Verify setting the"
				+ " ReportingPeriod value to 60 using the WEBPA parameter: "
				+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD);
		LOGGER.info("7. Perform reboot  On device");
		LOGGER.info("8. Verify whether WebPA is Up and Running in the Device after reboot");
		LOGGER.info("9. Verify the value is retained for " + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT
				+ " after performing Reboot .");
		LOGGER.info("10. Verify the value is reverted to the default value 900 for "
				+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + " after performing Reboot .");
		LOGGER.info("11. Verify the value is reverted to the default value 900 for "
				+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + " after performing Reboot .");
		LOGGER.info("12. Verify the value is retained for " + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS
				+ " after performing Reboot .");
		LOGGER.info("13. Verify the value is reverted to the default value 900 for "
				+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD
				+ " after performing Reboot .");
		LOGGER.info("14. Verify the value is reverted to the default value 900 for "
				+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD
				+ " after performing Reboot .");
		LOGGER.info(
				"POST-CONDITION 1 : Verify Reverting the default values by setting the  InterfaceDevicesEnable, InterfaceDevice ReportingPeriod,InterfaceDevice PollingPeriod, "
						+ "RadioInterfaceStatisticsEnable,RadioInterfaceStatistics ReportingPeriod,RadioInterfaceStatistics PollingPeriod value to Default Values Retrieved using the WEBPA");
		LOGGER.info("#######################################################################################");
		try {
			stepNum = "S1";
			errorMessage = "Failed to enable InterfaceDevicesWifi harvester report using webpa parameter: "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify Getting the Default Value for the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT
					+ " and  Enable InterfaceDevicesWifi harvester"
					+ " report by setting the value as true via WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
			LOGGER.info("STEP 1: ACTION : Get the WEBPA GET response for "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT
					+ " and the WEBPA SET Curl command should get executed successfully for "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT + " with value as true");
			LOGGER.info(
					"STEP 1: EXPECTED : The WEBPA GET should be successful with the default value retrieved. The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
			LOGGER.info("**********************************************************************************");
			interfaceDeviceWifiEnabledValue = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
			LOGGER.info("INTERFACE DEVICE WIFI ENABLE STATUS RETRIEVED FROM WEBPA GET IS : "
					+ interfaceDeviceWifiEnabledValue);
			if (CommonUtils.isNotEmptyOrNull(interfaceDeviceWifiEnabledValue) && (CommonUtils
					.patternSearchFromTargetString(interfaceDeviceWifiEnabledValue, BroadBandTestConstants.FALSE)
					|| CommonUtils.patternSearchFromTargetString(interfaceDeviceWifiEnabledValue,
							BroadBandTestConstants.TRUE))) {
				WebPaParameter InterfaceDeviceWifiEnabledParameter = new WebPaParameter();
				InterfaceDeviceWifiEnabledParameter
						.setName(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
				InterfaceDeviceWifiEnabledParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
				InterfaceDeviceWifiEnabledParameter.setValue(interfaceDeviceWifiEnabledValue);
				webPaParameters.add(InterfaceDeviceWifiEnabledParameter);

				LOGGER.info("GOING TO SET VALUE AS TRUE FOR THE INTERFACE DEVICE WIFI REPORT WEBPA PARAMETER");
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT, BroadBandTestConstants.CONSTANT_3,
						BroadBandTestConstants.TRUE, BroadBandTestConstants.THREE_MINUTES,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} else {
				errorMessage = "INTERFACE DEVICE WIFI ENABLE STATUS RETRIEVED FROM WEBPA GET IS NOT WITHIN THE APPLICABLE "
						+ "VALUES (True or False).";
			}
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : Default value for InterfaceDeviceWifi.Enabled is retrieved successfully and Setting the InterfaceDeviceWifi.Enabled to True is Successful via WEBPA");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S2";
			errorMessage = "Failed to set ReportingPeriod harvester report value to 900 using WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify Getting the Default Value for the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + " and Verify setting the "
					+ "ReportingPeriod value to 900 using the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD);
			LOGGER.info("STEP 2: ACTION : Get the WEBPA GET response for "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD
					+ " and The WEBPA SET Curl command should get executed " + "successfully for "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD);
			LOGGER.info(
					"STEP 2: EXPECTED : The WEBPA GET should be successful with the default value retrieved and The WEBPA SET Curl command should get executed successfully "
							+ "with success message and status as 200.");
			LOGGER.info("**********************************************************************************");
			reportingPeriodValue = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD);
			LOGGER.info(
					"INTERFACE DEVICE REPORTING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : " + reportingPeriodValue);
			if (CommonUtils.isNotEmptyOrNull(reportingPeriodValue)
					&& BroadBandTestConstants.APPLICABLE_VALUES_FOR_HARVESTER_DEVICE_WIFI_PARAMETERS
							.contains(reportingPeriodValue)) {
				WebPaParameter reportingPeriodParameter = new WebPaParameter();
				reportingPeriodParameter.setName(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD);
				reportingPeriodParameter.setDataType(BroadBandTestConstants.CONSTANT_2);
				reportingPeriodParameter.setValue(reportingPeriodValue);
				webPaParameters.add(reportingPeriodParameter);
				LOGGER.info("GOING TO SET VALUE AS 900 FOR THE INTERFACE DEVICE REPORTING PERIOD WEBPA PARAMETER");
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD,
						BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.CONSTANT_NINE_HUNDRED,
						BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} else {
				errorMessage = "INTERFACE DEVICE REPORTING PERIOD VALUE RETRIEVED FROM WEBPA GET IS NOT WITHIN THE APPLICABLE VALUES (5,10,15,30,60,300,900,1800,3600,10800,21600,43200,86400).";
			}
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : Default value for InterfaceDevicesWifi ReportingPeriod is retrieved successfully and Setting the InterfaceDevicesWifi ReportingPeriod to 900 is Successful via WEBPA");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S3";
			errorMessage = "Failed to set PollingPeriod harvester report value to 900 using WEBPA parameter Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.PollingPeriod.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify Getting the Default Value for the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + " and Verify setting the "
					+ "PollingPeriod value to 900 using the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD);
			LOGGER.info("STEP 3: ACTION : Get the WEBPA GET response for "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD
					+ " and The WEBPA SET Curl command should get executed successfully " + "for "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD);
			LOGGER.info(
					"STEP 3: EXPECTED : The WEBPA GET should be successful with the default value retrieved and the WEBPA SET Curl command should get executed successfully "
							+ "with success message and status as 200.");
			LOGGER.info("**********************************************************************************");
			pollingPeriodValue = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD);
			LOGGER.info("INTERFACE DEVICE POLLING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : " + pollingPeriodValue);
			if (CommonUtils.isNotEmptyOrNull(pollingPeriodValue)
					&& BroadBandTestConstants.APPLICABLE_VALUES_FOR_HARVESTER_DEVICE_WIFI_PARAMETERS
							.contains(pollingPeriodValue)) {
				WebPaParameter pollingPeriodParameter = new WebPaParameter();
				pollingPeriodParameter.setName(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD);
				pollingPeriodParameter.setDataType(BroadBandTestConstants.CONSTANT_2);
				pollingPeriodParameter.setValue(pollingPeriodValue);
				webPaParameters.add(pollingPeriodParameter);
				LOGGER.info("GOING TO SET VALUE AS 900 FOR THE INTERFACE DEVICE POLLING PERIOD WEBPA PARAMETER");
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD,
						BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.CONSTANT_NINE_HUNDRED,
						BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} else {
				errorMessage = "INTERFACE DEVICE POLLING PERIOD VALUE RETRIEVED FROM WEBPA GET IS NOT WITHIN THE APPLICABLE VALUES (5,10,15,30,60,300,900,1800,3600,10800,21600,43200,86400).";
			}
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Default value for InterfaceDevicesWifi PollingPeriod is retrieved successfully and Setting the InterfaceDevicesWifi PollingPeriod to 900 is Successful via WEBPA");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S4";
			errorMessage = "Failed to set RadioInterfaceStatistics.Enabled value to true using WEBPA parameter Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.Enabled";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify Getting the Default Value for the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS + " and Verify "
					+ "setting the RadioInterfaceStatistics.Enabled value to true using the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
			LOGGER.info("STEP 4: ACTION : Get the WEBPA GET response for "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS
					+ " and The WEBPA SET Curl command should get " + "executed successfully for "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
			LOGGER.info(
					"STEP 4: EXPECTED : The WEBPA GET should be successful with the default value retrieved and The WEBPA SET Curl command should get executed successfully "
							+ "with success message and status as 200.");
			LOGGER.info("**********************************************************************************");
			radioInterfaceStatisticsEnabledValue = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
			LOGGER.info("RADIO INTERFACE STATISTICS ENABLE VALUE RETRIEVED FROM WEBPA GET IS : "
					+ radioInterfaceStatisticsEnabledValue);
			if (CommonUtils.isNotEmptyOrNull(radioInterfaceStatisticsEnabledValue) && (CommonUtils
					.patternSearchFromTargetString(radioInterfaceStatisticsEnabledValue, BroadBandTestConstants.FALSE)
					|| CommonUtils.patternSearchFromTargetString(radioInterfaceStatisticsEnabledValue,
							BroadBandTestConstants.TRUE))) {
				WebPaParameter radioInterfaceStatisticsEnabledParameter = new WebPaParameter();
				radioInterfaceStatisticsEnabledParameter
						.setName(BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
				radioInterfaceStatisticsEnabledParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
				radioInterfaceStatisticsEnabledParameter.setValue(radioInterfaceStatisticsEnabledValue);
				webPaParameters.add(radioInterfaceStatisticsEnabledParameter);
				LOGGER.info("GOING TO SET VALUE AS TRUE FOR THE RADIO INTERFACE STATISTICS ENABLE WEBPA PARAMETER");
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS, BroadBandTestConstants.CONSTANT_3,
						BroadBandTestConstants.TRUE, BroadBandTestConstants.THREE_MINUTES,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} else {
				errorMessage = "RADIO INTERFACE STATISTICS ENABLE VALUE RETRIEVED FROM WEBPA GET IS NOT WITHIN THE APPLICABLE "
						+ "VALUES (True or False).";
			}
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Default value for RadioInterfaceStatistics.Enabled is retrieved successfully and Setting the RadioInterfaceStatistics.Enabled to True is Successful via WEBPA");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S5";
			errorMessage = "Failed to set PollingPeriod value to 60 using WEBPA parameter Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.PollingPeriod";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify Getting the Default Value for the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD + ""
					+ "and Verify setting the PollingPeriod value to 60 using the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD);
			LOGGER.info("STEP 5: ACTION : Get the WEBPA GET response for "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD
					+ " and The WEBPA SET Curl command " + "should get executed successfully for "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD + "");
			LOGGER.info(
					"STEP 5: EXPECTED : The WEBPA GET should be successful with the default value retrieved and The WEBPA SET Curl command should get executed successfully "
							+ "with success message and status as 200.");
			LOGGER.info("**********************************************************************************");
			interfaceStatisticsPollingPeriodValue = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD);
			LOGGER.info("RADIO INTERFACE STATISTICS POLLING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : "
					+ interfaceStatisticsPollingPeriodValue);
			if (CommonUtils.isNotEmptyOrNull(interfaceStatisticsPollingPeriodValue)
					&& BroadBandTestConstants.APPLICABLE_VALUES_FOR_HARVESTER_DEVICE_WIFI_PARAMETERS
							.contains(interfaceStatisticsPollingPeriodValue)) {
				WebPaParameter interfaceStatisticsPollingPeriodParameter = new WebPaParameter();
				interfaceStatisticsPollingPeriodParameter
						.setName(BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD);
				interfaceStatisticsPollingPeriodParameter.setDataType(BroadBandTestConstants.CONSTANT_2);
				interfaceStatisticsPollingPeriodParameter.setValue(interfaceStatisticsPollingPeriodValue);
				webPaParameters.add(interfaceStatisticsPollingPeriodParameter);
				LOGGER.info(
						"GOING TO SET VALUE AS 60 FOR THE RADIO INTERFACE STATISTICS POLLING PERIOD WEBPA PARAMETER");
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD,
						BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_60,
						BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} else {
				errorMessage = "RADIO INTERFACE STATISTICS POLLING PERIOD VALUE RETRIEVED FROM WEBPA GET IS NOT WITHIN THE APPLICABLE VALUES (5,10,15,30,60,300,900,1800,3600,10800,21600,43200,86400).";
			}
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : Default value for RadioInterfaceStatistics PollingPeriod is retrieved successfully and Setting the RadioInterfaceStatistics PollingPeriod to 60 is Successful via WEBPA");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S6";
			errorMessage = "Failed to set ReportingPeriod value to 60 using WEBPA parameter Device.X_RDKCENTRAL-COM_Report.RadioInterfaceStatistics.ReportingPeriod";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify Getting the Default Value for the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD + " "
					+ "and Verify setting the ReportingPeriod value to 60 using the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD);
			LOGGER.info("STEP 6: ACTION : Get the WEBPA GET response for "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD
					+ " and The WEBPA SET Curl command " + "should get executed successfully for "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD);
			LOGGER.info(
					"STEP 6: EXPECTED : The WEBPA GET should be successful with the default value retrieved and The WEBPA SET Curl command should get executed successfully "
							+ "with success message and status as 200.");
			LOGGER.info("**********************************************************************************");
			interfaceStatisticsReportingPeriodValue = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD);
			LOGGER.info("RADIO INTERFACE STATISTICS REPORTING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : "
					+ interfaceStatisticsReportingPeriodValue);
			if (CommonUtils.isNotEmptyOrNull(interfaceStatisticsReportingPeriodValue)
					&& BroadBandTestConstants.APPLICABLE_VALUES_FOR_HARVESTER_DEVICE_WIFI_PARAMETERS
							.contains(interfaceStatisticsReportingPeriodValue)) {
				WebPaParameter interfaceStatisticsReportingPeriodParameter = new WebPaParameter();
				interfaceStatisticsReportingPeriodParameter
						.setName(BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD);
				interfaceStatisticsReportingPeriodParameter.setDataType(BroadBandTestConstants.CONSTANT_2);
				interfaceStatisticsReportingPeriodParameter.setValue(interfaceStatisticsReportingPeriodValue);
				webPaParameters.add(interfaceStatisticsReportingPeriodParameter);
				LOGGER.info(
						"GOING TO SET VALUE AS 60 FOR THE RADIO INTERFACE STATISTICS REPORTING PERIOD WEBPA PARAMETER");
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD,
						BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_60,
						BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} else {
				errorMessage = "RADIO INTERFACE STATISTICS REPORTING PERIOD VALUE RETRIEVED FROM WEBPA GET IS NOT WITHIN THE APPLICABLE VALUES (5,10,15,30,60,300,900,1800,3600,10800,21600,43200,86400).";
			}
			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL : Default value for RadioInterfaceStatistics ReportingPeriod is retrieved successfully and Setting the RadioInterfaceStatistics ReportingPeriod to 60 is Successful via WEBPA");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S7";
			errorMessage = "Unable to reboot device";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Perform reboot  On device");
			LOGGER.info("STEP 7: ACTION : Execute command \"reboot\" on the device");
			LOGGER.info("STEP 7: EXPECTED : Reboot of the device must be performed successfully");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Device is Rebooted Successfully.");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S8";
			errorMessage = "Webpa is not Up and Running";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify whether WebPA is Up and Running in the Device after reboot");
			LOGGER.info(
					"STEP 8: ACTION : Verifying Successful webpa Get response ,in case of failure rechecking for 8 minutes");
			LOGGER.info("STEP 8: EXPECTED : WebPA should be Up and Running in the Device.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			if (status) {
				LOGGER.info("STEP 8: ACTUAL : WebPA is Up and Running in the Device");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S9";
			errorMessage = "WEBPA  value retrieved is not same as the value set before device reboot.(Value Not Retained after reboot)";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify the value is retained for "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT + "" + " after performing Reboot .");
			LOGGER.info("STEP 9: ACTION : Get the WEBPA GET response for the WEBPA Parameter: "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
			LOGGER.info(
					"STEP 9: EXPECTED : The WEBPA Command should get executed successfully with value retrieved as true which "
							+ "confirms the value is retained after reboot.");
			LOGGER.info("**********************************************************************************");
			interfaceDeviceEnableStatusPostReboot = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
			LOGGER.info("INTERFACE DEVICE WIFI ENABLE STATUS RETRIEVED FROM WEBPA GET IS : "
					+ interfaceDeviceEnableStatusPostReboot);
			status = CommonUtils.patternSearchFromTargetString(interfaceDeviceEnableStatusPostReboot,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 9: ACTUAL : WEBPA response retrieved is True for InterfaceDevicesWifi Enabled"
						+ " which confirms the value is retained even after performing reboot");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S10";
			errorMessage = "WEBPA  value retrieved is not reverted back to default value as 900.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Verify the value is reverted to the default value 900 for " + ""
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + " after performing Reboot .");
			LOGGER.info("STEP 10: ACTION : Get the WEBPA GET response for  for the WEBPA parameter "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + "");
			LOGGER.info("STEP 10: EXPECTED : The Command should get executed successfully with value retrieved as 900"
					+ " which confirms the value is reverted to default value after performing reboot.");
			LOGGER.info("**********************************************************************************");
			interfaceDeviceReportingPeriodPostReboot = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD);
			LOGGER.info("INTERFACE DEVICE WIFI REPORTING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : "
					+ interfaceDeviceReportingPeriodPostReboot);
			status = CommonUtils.patternSearchFromTargetString(interfaceDeviceReportingPeriodPostReboot,
					BroadBandTestConstants.CONSTANT_NINE_HUNDRED);
			if (status) {
				LOGGER.info(
						"STEP 10: ACTUAL : WEBPA response retrieved is 900 for InterfaceDevicesWifi ReportingPeriod "
								+ "which confirms the value is reverted to default value even after performing reboot");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S11";
			errorMessage = "WEBPA  value retrieved for Polling Period is not reverted back to default value as 900.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Verify the value is reverted to the default value 900 for "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + ""
					+ " after performing Reboot .");
			LOGGER.info("STEP 11: ACTION : Get the WEBPA GET response for WEBPA Parameter: "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD);
			LOGGER.info("STEP 11: EXPECTED : The Command should get executed successfully with value retrieved as 900"
					+ " which confirms the value is reverted to default value after performing reboot.");
			LOGGER.info("**********************************************************************************");
			interfaceDevicePollingPeriodAfterReboot = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD);
			LOGGER.info("INTERFACE DEVICE WIFI POLLING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : "
					+ interfaceDevicePollingPeriodAfterReboot);
			status = CommonUtils.patternSearchFromTargetString(interfaceDevicePollingPeriodAfterReboot,
					BroadBandTestConstants.CONSTANT_NINE_HUNDRED);
			if (status) {
				LOGGER.info("STEP 11: ACTUAL : WEBPA response retrieved is 900 for InterfaceDevicesWifi PollingPeriod "
						+ "which confirms the value is reverted to default value even after performing reboot");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S12";
			errorMessage = "WEBPA  value retrieved is not same as the value set before device reboot.(Value Not Retained after reboot)";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Verify the value is retained for "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS + " " + "after performing Reboot .");
			LOGGER.info("STEP 12: ACTION : Get the WEBPA GET response for the WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
			LOGGER.info("STEP 12: EXPECTED : The Command should get executed successfully with value retrieved as true "
					+ "which confirms that the Enable/Disable status configuration for this report is retained even after a reboot ");
			LOGGER.info("**********************************************************************************");
			interfaceStatisticsEnableStatusAfterReboot = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
			LOGGER.info("RAIDO INTERFACE STATISTICS ENABLED STATUS RETRIEVED FROM WEBPA GET IS : "
					+ interfaceStatisticsEnableStatusAfterReboot);
			status = CommonUtils.patternSearchFromTargetString(interfaceStatisticsEnableStatusAfterReboot,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 12: ACTUAL : WEBPA response retrieved is True for RadioInterfaceStatistics Enabled "
						+ "which confirms the value is retained even after performing reboot");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S13";
			errorMessage = "WEBPA  value retrieved for PollingPeriod is not reverted back to default value as 900.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Verify the value is reverted to the default value 900 for " + ""
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD
					+ " after performing Reboot .");
			LOGGER.info("STEP 13: ACTION : Get the WEBPA GET response for  WEBPA parameter: "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD);
			LOGGER.info("STEP 13: EXPECTED : The Command should get executed successfully with value retrieved as 900"
					+ " which confirms the value is reverted to default value after performing reboot.");
			LOGGER.info("**********************************************************************************");
			interfaceStatisticsPollingPeriodPostReboot = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD);
			LOGGER.info("RADIO INTERFACE STATISTICS POLLING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : "
					+ interfaceStatisticsPollingPeriodPostReboot);
			if (!DeviceModeHandler.isFibreDevice(device)) {
				status = CommonMethods.isNotNull(interfaceStatisticsPollingPeriodPostReboot)
						&& CommonUtils.patternSearchFromTargetString(interfaceStatisticsPollingPeriodPostReboot,
								BroadBandTestConstants.CONSTANT_NINE_HUNDRED);
			} else {
				status = CommonMethods.isNotNull(interfaceStatisticsPollingPeriodPostReboot)
						&& CommonUtils.patternSearchFromTargetString(interfaceStatisticsPollingPeriodPostReboot,
								BroadBandTestConstants.STRING_300);
			}
			if (status) {
				LOGGER.info(
						"STEP 13: ACTUAL :  WEBPA response retrieved is 900 for RadioInterfaceStatistics PollingPeriod "
								+ "which confirms the value is reverted to default value even after performing reboot");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S14";
			errorMessage = "WEBPA  value retrieved for ReportingPeriod is not reverted back to default value as 900.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 14: DESCRIPTION : Verify the value is reverted to the default value 900 for" + " "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD
					+ " even after performing Reboot .");
			LOGGER.info("STEP 14: ACTION : Get the WEBPA GET response for the WEBPA Parameter "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD);
			LOGGER.info("STEP 14: EXPECTED : The Command should get executed successfully with value retrieved as 900"
					+ " which confirms the value is reverted to default value after performing reboot.");
			LOGGER.info("**********************************************************************************");
			interfaceStatisticsReportingPeriodPostReboot = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD);
			LOGGER.info("RADIO INTERFACE STATISTICS REPORTING PERIOD VALUE RETRIEVED FROM WEBPA GET IS : "
					+ interfaceStatisticsReportingPeriodPostReboot);
			if (!DeviceModeHandler.isFibreDevice(device)) {
				status = CommonMethods.isNotNull(interfaceStatisticsReportingPeriodPostReboot)
						&& CommonUtils.patternSearchFromTargetString(interfaceStatisticsReportingPeriodPostReboot,
								BroadBandTestConstants.CONSTANT_NINE_HUNDRED);
			} else {
				status = CommonMethods.isNotNull(interfaceStatisticsReportingPeriodPostReboot)
						&& CommonUtils.patternSearchFromTargetString(interfaceStatisticsReportingPeriodPostReboot,
								BroadBandTestConstants.STRING_300);
			}

			if (status) {
				LOGGER.info(
						"STEP 14: ACTUAL :  WEBPA response retrieved is 900 for RadioInterfaceStatistics ReportingPeriod "
								+ "which confirms the value is reverted to default value even after performing reboot");
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info(
					"POST-CONDITION : DESCRIPTION : Verify Reverting the default values by setting the  InterfaceDevicesEnable,"
							+ " InterfaceDevice ReportingPeriod,InterfaceDevice PollingPeriod, RadioInterfaceStatisticsEnable,RadioInterfaceStatistics.ReportingPeriod,RadioInterfaceStatistics.PollingPeriod "
							+ "value to Default Values Retrieved using the WEBPA ");
			LOGGER.info("POST-CONDITION : ACTION : Set to default Values for the WEBPA Parameters :  "
					+ "                     1. " + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT + "  2. "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + " " + "3. "
					+ BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + "  4. "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS + "  5. "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD + " " + "6. "
					+ BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD + "");
			LOGGER.info(
					"POST-CONDITION : EXPECTED : Setting the Default values to the WEBPA parameter should be successful.");
			resultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
					webPaParameters);
			status = resultObject.isStatus();
			errorMessage = resultObject.getErrorMessage();
			if (status) {
				LOGGER.info(
						"POST-CONDITION : ACTUAL : All Webpa Parameters are reverted back to its default values successfully.");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed" + errorMessage);
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-HARV-6009");
	}
	
    /**
     * Verify PollingPeriod & ReportingPeriod values for InterfaceDevicesWifi Report and RadioInterfaceStatistics
     * Report.
     * <ol>
     * <li>PRE CONDITION 1: Verify whether WebPA is Up and Running in the Device.</li>
     * <li>PRE CONDITION 2: Verify setting the default values for all the Webpa Params.</li>
     * <li>Verify PollingPeriod is set to 60 for InterfaceDevicesWifi report.</li>
     * <li>Verify ReportingPeriod is cannot be faster than the current PollingPeriod for InterfaceDevicesWifi
     * Report.</li>
     * <li>Verify ReportingPeriod is can only be set slower than the current PollingPeriod for InterfaceDevicesWifi
     * Report.</li>
     * <li>Verify PollingPeriod is set to 60 for RadioInterfaceStatistics report.</li>
     * <li>Verify ReportingPeriod is cannot be faster than the current PollingPeriod for RadioInterfaceStatistics
     * Report.</li>
     * <li>Verify ReportingPeriod can only be set slower than the current PollingPeriod for RadioInterfaceStatistics
     * Report.</li>
     * <li>Verify PollingPeriod is set to 15 for InterfaceDevicesWifi report.</li>
     * <li>Verify ReportingPeriod is set to 60 for InterfaceDevicesWifi report.</li>
     * <li>Verify PollingPeriod is cannot be slower than the current ReportingPeriod for InterfaceDevicesWifi
     * Report.</li>
     * <li>Verify PollingPeriod can be set faster than the current ReportingPeriod for InterfaceDevicesWifi Report.</li>
     * <li>Verify PollingPeriod is set to 15 for RadioInterfaceStatistics report.</li>
     * <li>Verify ReportingPeriod is set to 60 for RadioInterfaceStatistics report.</li>
     * <li>Verify PollingPeriod is cannot be slower than the current ReportingPeriod for InterfaceDevicesWifi
     * Report.</li>
     * <li>Verify PollingPeriod can be set faster than the current ReportingPeriod for InterfaceDevicesWifi Report.</li>
     * <li>POST CONDITION 1: Verify Reverting the default values for all webPa Params.</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * 
     * @author prashant.mishra12
     * @refactor Rakesh C N
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-HARV-6010")
    public void testToVerifyPollingAndReportingPeriodForInterfaceDevicesWifiAndRadioInterfaceStatistics(Dut device) {
	// Variable Declaration begins
	boolean status = false;
	String testCaseId = "TC-RDKB-HARV-610";
	String stepNum = "";
	String errorMessage = "";
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-HARV-6010");
	LOGGER.info(
		"TEST DESCRIPTION: Verify PollingPeriod & ReportingPeriod values for InterfaceDevicesWifi Report and RadioInterfaceStatistics Report.");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE CONDITION 1. Verify whether WebPA is Up and Running in the Device.");
	LOGGER.info("PRE CONDITION 2. Verify setting the default values for all the Webpa Params.");
	LOGGER.info("1. Verify PollingPeriod is set to 60 for InterfaceDevicesWifi report.");
	LOGGER.info(
		"2. Verify ReportingPeriod is cannot be faster than the current PollingPeriod for InterfaceDevicesWifi Report.");
	LOGGER.info(
		"3. Verify ReportingPeriod is can only be set slower than the current PollingPeriod for InterfaceDevicesWifi Report.");
	LOGGER.info("4. Verify PollingPeriod is set to 60 for RadioInterfaceStatistics report.");
	LOGGER.info(
		"5. Verify ReportingPeriod is cannot be faster than the current PollingPeriod for RadioInterfaceStatistics Report.");
	LOGGER.info(
		"6. Verify ReportingPeriod can only be set slower than the current PollingPeriod for RadioInterfaceStatistics Report.");
	LOGGER.info("7. Verify PollingPeriod is set to 15 for InterfaceDevicesWifi report.");
	LOGGER.info("8. Verify ReportingPeriod is set to 60 for InterfaceDevicesWifi report.");
	LOGGER.info(
		"9. Verify PollingPeriod is cannot be slower than the current ReportingPeriod for InterfaceDevicesWifi Report.");
	LOGGER.info(
		"10. Verify PollingPeriod can be set faster than the current ReportingPeriod for InterfaceDevicesWifi Report.");
	LOGGER.info("11. Verify PollingPeriod is set to 15 for RadioInterfaceStatistics report.");
	LOGGER.info("12. Verify ReportingPeriod is set to 60 for RadioInterfaceStatistics report.");
	LOGGER.info(
		"13. Verify PollingPeriod is cannot be slower than the current ReportingPeriod for InterfaceDevicesWifi Report.");
	LOGGER.info(
		"14. Verify PollingPeriod can be set faster than the current ReportingPeriod for InterfaceDevicesWifi Report.");
	LOGGER.info("POST CONDITION 1. Verify Reverting the default values for all webPa Params.");

	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");

	    errorMessage = "Webpa is not Up and  not Running.";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Verify whether WebPA is Up and Running in the Device.");
	    LOGGER.info(
		    "PRE-CONDITION 1 : ACTION : Verifying Successful webpa Get response ,in case of failure rechecking for 8 minutes.");
	    LOGGER.info("PRE-CONDITION 1 : EXPECTED : WebPA should be Up and Running in the Device.");
	    LOGGER.info("#######################################################################################");
	    try {
		status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 1 : ACTUAL : WebPA is Up and Running in Device.");
	    } else {
		LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
	    }

	    errorMessage = "Unable to set default values for all the WebPa Params.";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : Verify setting the default values for all the Webpa Params.");
	    LOGGER.info("POST-CONDITION 2 : ACTION : Set to default Values for the WEBPA Parameters :  "
		    + "                     1. " + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT + "  2. "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + " " + "3. "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + "  4. "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS + "  5. "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD + " " + "6. "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD + "");
	    LOGGER.info(
		    "PRE-CONDITION 2 : EXPECTED : Setting the Default values for all the WEBPA parameter should be successful.");
	    LOGGER.info("#######################################################################################");
	    try {
		if (verifyWebPaParamsAlredySetToDefault(device)) {
		    status = enableOrDisableHarvesterReport(BroadBandTestConstants.BOOLEAN_VALUE_TRUE, device);
		} else {
		    status = CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device);
		    if (status) {
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			if (status) {
			    status = verifyWebPaParamsAlredySetToDefault(device) && enableOrDisableHarvesterReport(
				    BroadBandTestConstants.BOOLEAN_VALUE_TRUE, device);
			}
		    }
		}

	    } catch (Exception exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info(
			"PRE-CONDITION 2 : ACTUAL : Setting all the Webpa Params to default is Successful via WEBPA");
	    } else {
		LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
	    }

	    stepNum = "S1";
	    errorMessage = "Unable to set the value of PollingPeriod as 60 for InterfaceDevicesWifi report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify Polling Period is set to 60 for InterfaceDevicesWifi report.");
	    LOGGER.info("STEP 1: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + " with value as 60.");
	    LOGGER.info(
		    "STEP 1: EXPECTED : The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_60, BroadBandTestConstants.THREE_MINUTES,
		    BroadBandTestConstants.TWO_MINUTES);
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Setting the InterfaceDevicesWifi PollingPeriod to 60 is Successful via WEBPA.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "Unable to verify setting ReportingPeriod faster than Polling Period for InterfaceDevicesWifi report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify ReportingPeriod is cannot be faster than the current PollingPeriod for InterfaceDevicesWifi Report.");
	    LOGGER.info("STEP 2: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + " with value as 30.");
	    LOGGER.info(
		    "STEP 2: EXPECTED : The WEBPA SET Curl command shouldn't get executed successfully with failure message and status as 520.");
	    LOGGER.info("**********************************************************************************");
	    status = !BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_30, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Setting ReportingPeriod faster than PollingPeriod for InterfaceDevicesWifi report verified successfully.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S3";
	    errorMessage = "Unable to verify setting ReportingPeriod slower than Polling Period for InterfaceDevicesWifi report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify ReportingPeriod is can only be set slower than the current PollingPeriod for InterfaceDevicesWifi Report.");
	    LOGGER.info("STEP 3: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + " with value as 300.");
	    LOGGER.info(
		    "STEP 3: EXPECTED : The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_300, BroadBandTestConstants.TWO_MINUTES,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Setting ReportingPeriod slower than Polling Period for InterfaceDevicesWifi report verified successfully.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S4";
	    errorMessage = "Unable to set the value of PollingPeriod as 60 for RadioInterfaceStatistics report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify Polling Period is set to 60 for RadioInterfaceStatistics report.");
	    LOGGER.info("STEP 4: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD + " with value as 60.");
	    LOGGER.info(
		    "STEP 4: EXPECTED : The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_60,
		    BroadBandTestConstants.TWO_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Setting the RadioInterfaceStatistics PollingPeriod to 60 is Successful via WEBPA.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S5";
	    errorMessage = "Unable to verify setting ReportingPeriod faster than Polling Period for RadioInterfaceStatistics report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify ReportingPeriod is cannot be faster than the current PollingPeriod for RadioInterfaceStatistics Report.");
	    LOGGER.info("STEP 5: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD + " with value as 30.");
	    LOGGER.info(
		    "STEP 5: EXPECTED : The WEBPA SET Curl command shouldn't get executed successfully with failure message and status as 520.");
	    LOGGER.info("**********************************************************************************");
	    status = !BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_30,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : Setting ReportingPeriod faster than PollingPeriod for RadioInterfaceStatistics report verified successfully.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S6";
	    errorMessage = "Unable to verify setting ReportingPeriod slower than Polling Period for RadioInterfaceStatistics report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify ReportingPeriod can only be set slower than the current PollingPeriod for RadioInterfaceStatistics Report.");
	    LOGGER.info("STEP 6: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD
		    + " with value as 300.");
	    LOGGER.info(
		    "STEP 6: EXPECTED : The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_300,
		    BroadBandTestConstants.TWO_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Setting ReportingPeriod slower than PollingPeriod for RadioInterfaceStatistics report verified successfully.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S7";
	    errorMessage = "Unable to set the value of PollingPeriod as 15 for InterfaceDevicesWifi report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify PollingPeriod is set to 15 for InterfaceDevicesWifi report.");
	    LOGGER.info("STEP 7: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + " with value as 15.");
	    LOGGER.info(
		    "STEP 7: EXPECTED : The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_FIFTEEN, BroadBandTestConstants.TWO_MINUTES,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL : Setting the InterfaceDevicesWifi PollingPeriod to 60 is Successful via WEBPA.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S8";
	    errorMessage = "Unable to set the value of ReportingPeriod as 60 for InterfaceDevicesWifi report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify ReportingPeriod is set to 60 for InterfaceDevicesWifi report.");
	    LOGGER.info("STEP 8: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + " with value as 60.");
	    LOGGER.info(
		    "STEP 8: EXPECTED : The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_60, BroadBandTestConstants.TWO_MINUTES,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : Setting the InterfaceDevicesWifi ReportingPeriod to 60 is Successful via WEBPA.");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S9";
	    errorMessage = "Unable to verify setting PollingPeriod slower than ReportingPeriod for InterfaceDevicesWifi report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION :Verify PollingPeriod is cannot be slower than the current ReportingPeriod for InterfaceDevicesWifi Report.");
	    LOGGER.info("STEP 9: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + " with value as 300.");
	    LOGGER.info(
		    "STEP 9: EXPECTED : The WEBPA SET Curl command shouldn't get executed successfully with failure message and status as 520.");
	    LOGGER.info("**********************************************************************************");
	    status = !BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_300, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : Setting PollingPeriod slower than ReportingPeriod for InterfaceDevicesWifi report verified successfully.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S10";
	    errorMessage = "Unable to verify setting PollingPeriod faster than ReportingPeriod for InterfaceDevicesWifi report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION :Verify PollingPeriod can be set faster than the current ReportingPeriod for InterfaceDevicesWifi Report.");
	    LOGGER.info("STEP 10: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + " with value as 30.");
	    LOGGER.info(
		    "STEP 10: EXPECTED : The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_FIVE, BroadBandTestConstants.TWO_MINUTES,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : Setting PollingPeriod faster than ReportingPeriod for InterfaceDevicesWifi report verified successfully.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S11";
	    errorMessage = "Unable to set the value of PollingPeriod as 15 for RadioInterfaceStatistics report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Verify Polling Period is set to 15 for RadioInterfaceStatistics report.");
	    LOGGER.info("STEP 11: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD + " with value as 15.");
	    LOGGER.info(
		    "STEP 11: EXPECTED : The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_FIFTEEN,
		    BroadBandTestConstants.TWO_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL : Setting the RadioInterfaceStatistics PollingPeriod to 15 is Successful via WEBPA.");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S12";
	    errorMessage = "Unable to set the value of ReportingPeriod as 60 for RadioInterfaceStatistics report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify ReportingPeriod is set to 60 for RadioInterfaceStatistics report.");
	    LOGGER.info("STEP 12: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD + " with value as 60.");
	    LOGGER.info(
		    "STEP 12: EXPECTED : The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_60,
		    BroadBandTestConstants.TWO_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 12: ACTUAL : Setting the RadioInterfaceStatistics ReportingPeriod to 60 is Successful via WEBPA.");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S13";
	    errorMessage = "Unable to verify setting PollingPeriod slower than ReportingPeriod for InterfaceDevicesWifi report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION :Verify PollingPeriod is cannot be slower than the current ReportingPeriod for InterfaceDevicesWifi Report.");
	    LOGGER.info("STEP 13: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD + " with value as 300.");
	    LOGGER.info(
		    "STEP 13: EXPECTED : The WEBPA SET Curl command shouldn't get executed successfully with failure message and status as 520.");
	    LOGGER.info("**********************************************************************************");
	    status = !BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_300,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 13: ACTUAL : Setting PollingPeriod slower than ReportingPeriod for InterfaceDevicesWifi report verified successfully.");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S14";
	    errorMessage = "Unable to verify setting PollingPeriod faster than ReportingPeriod for InterfaceDevicesWifi report.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION :Verify PollingPeriod can be set faster than the current ReportingPeriod for InterfaceDevicesWifi Report.");
	    LOGGER.info("STEP 14: ACTION : Execute WEBPA SET Curl command: "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD + " with value as 30.");
	    LOGGER.info(
		    "STEP 14: EXPECTED : The WEBPA SET Curl command should get executed successfully with success message and status as 200.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_FIVE,
		    BroadBandTestConstants.TWO_MINUTES, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 14: ACTUAL : Setting PollingPeriod faster than ReportingPeriod for InterfaceDevicesWifi report verified successfully.");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    errorMessage = "Unable to revert the default values of all WebPa Params.";
	    status = false;
	    LOGGER.info("POST-CONDITION : DESCRIPTION : Verify Reverting the default values for all webPa Params.");
	    LOGGER.info("POST-CONDITION : ACTION : Set to default Values for the WEBPA Parameters :  "
		    + "                     1. " + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT + "  2. "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD + " " + "3. "
		    + BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD + "  4. "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS + "  5. "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD + " " + "6. "
		    + BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD + "");
	    LOGGER.info(
		    "POST-CONDITION : EXPECTED : Setting the Default values to the WEBPA parameter should be successful.");

	    LOGGER.info("Waiting for 4 minutes for setting default values.");
	    tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);
	    status = setReportingAndPollingPeriodToDefault(device)
		    && enableOrDisableHarvesterReport(BroadBandTestConstants.BOOLEAN_VALUE_FALSE, device);
	    if (status) {
		LOGGER.info(
			"POST-CONDITION : ACTUAL : All Webpa Parameters are reverted back to its default values successfully.");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed" + errorMessage);
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-HARV-6010");
    }
    
    /**
     * Method to verify all webPa Params are already at default value or not
     * 
     * @param device
     *            {@link Dut}
     * @return true if all values are already set to default values
     * @refactor Rakesh C N
     */
    public boolean verifyWebPaParamsAlredySetToDefault(Dut device) {
	LOGGER.debug("ENTERING METHOD verifyWebPaParamsAlredySetToDefault");
	// Variable declaration Starts
	boolean areSetToDefault = false;
	String errorMessage = "";
	String interfaceReportingPeriodDefaultValue = null;
	String radioReportingDefaultValue = null;
	String interfacePollingPeriodDefaultValue = null;
	String radioPollingDefaultValue = null;
	String currentInterfacePollingPeriod = null;
	String currentInterfaceReportingPeriod = null;
	String currentRadioPollingPeriod = null;
	String currentRadioReportingPeriod = null;
	// Variable declaration Ends
	try {
	    errorMessage = "Unable to get Default value of Polling and Reporting Period.";
	    /**
	     * Getting default value for InterfaceDevicesWiFi Reporting Period
	     */
	    interfaceReportingPeriodDefaultValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device,
		    tapEnv, BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_DEFAULT_REPORTING_PERIOD);
	    LOGGER.info("DEFAULT VALUE FOR INTERFACE DEVICE WIFI REPORTING PERIOD IS: "
		    + interfaceReportingPeriodDefaultValue);
	    /**
	     * Getting default value for RadioInterfaceStatistics Reporting Period
	     */
	    radioReportingDefaultValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_DEFAULT_REPORTING_PERIOD);
	    LOGGER.info(
		    "DEFAULT VALUE FOR RADIO INTERFACE STATISTICS REPORTING PERIOD IS: " + radioReportingDefaultValue);
	    /** Getting default value for InterfaceDevicesWiFi Polling Period */
	    interfacePollingPeriodDefaultValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_DEFAULT_POLLING_PERIOD);
	    LOGGER.info(
		    "DEFAULT VALUE FOR INTERFACE DEVICE WIFI POLLING PERIOD IS: " + interfacePollingPeriodDefaultValue);
	    /**
	     * Getting default value for RadioInterfaceStatistics Polling Period
	     */
	    radioPollingDefaultValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_DEFAULT_POLLING_PERIOD);
	    LOGGER.info("DEFAULT VALUE FOR RADIO INTERFACE STATISTICS POLLING PERIOD IS: " + radioPollingDefaultValue);

	    /** Verifying all WebPa Params Default value for not null */
	    if (CommonMethods.isNotNull(interfaceReportingPeriodDefaultValue)
		    && CommonMethods.isNotNull(radioReportingDefaultValue)
		    && CommonMethods.isNotNull(interfacePollingPeriodDefaultValue)
		    && CommonMethods.isNotNull(radioPollingDefaultValue)) {
		errorMessage = "Unable to get current value of Polling and Reporting Period.";
		/**
		 * Getting current value for InterfaceDevicesWiFi Polling Period
		 */
		currentInterfacePollingPeriod = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD);
		LOGGER.info(
			"CURRENT VALUE FOR INTERFACE DEVICE WIFI POLLING PERIOD IS: " + currentInterfacePollingPeriod);
		/**
		 * Getting current value for InterfaceDevicesWiFi Reporting Period
		 */
		currentInterfaceReportingPeriod = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device,
			tapEnv, BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD);
		LOGGER.info("CURRENT VALUE FOR INTERFACE DEVICE WIFI REPORTING PERIOD IS: "
			+ currentInterfaceReportingPeriod);
		/**
		 * Getting current value for RadioInterfaceStatistics Polling Period
		 */
		currentRadioPollingPeriod = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD);
		LOGGER.info(
			"CURRENT VALUE FOR RADIO INTERFACE STATISTICS POLLING PERIOD IS: " + currentRadioPollingPeriod);
		/**
		 * Getting current value for RadioInterfaceStatistics Reporting Period
		 */
		currentRadioReportingPeriod = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD);
		LOGGER.info("CURRENT VALUE FOR RADIO INTERFACE STATISTICS REPORTING PERIOD IS: "
			+ currentRadioReportingPeriod);

		if (CommonMethods.isNotNull(currentInterfacePollingPeriod)
			&& CommonMethods.isNotNull(currentInterfaceReportingPeriod)
			&& CommonMethods.isNotNull(currentRadioPollingPeriod)
			&& CommonMethods.isNotNull(currentRadioReportingPeriod)) {
		    areSetToDefault = currentInterfaceReportingPeriod.equals(interfaceReportingPeriodDefaultValue)
			    && currentRadioReportingPeriod.equals(radioReportingDefaultValue)
			    && currentInterfacePollingPeriod.equals(interfacePollingPeriodDefaultValue)
			    && currentRadioPollingPeriod.equals(radioPollingDefaultValue);
		}
	    }
	    if (areSetToDefault) {
		LOGGER.info("All WebPa Params are already set to default.");
	    } else {
		LOGGER.error(errorMessage);
	    }
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	LOGGER.debug("ENDING METHOD verifyWebPaParamsAlredySetToDefault");
	return areSetToDefault;
    }
    
    /**
     * Method to Enable or Disable InterfaceDevicesWifi & RadioInterfaceStatistics Harvester Reports
     * 
     * @param isEnableReport
     *            True or False based on Enable and Disable requirement
     * @param device
     *            {@link Dut}
     * @return true if enabled or disabled successfully
     * @refactor Rakesh C N
     */
    public boolean enableOrDisableHarvesterReport(boolean isEnableReport, Dut device) {
	LOGGER.debug("ENTERING METHOD enableOrDisableHarvesterReport");
	// Variable declaration Starts
	boolean enablerorDisableStatus = false;
	String errorMessage = "";
	WebPaParameter radioInterfaceStatisticsEnabledParameter = new WebPaParameter();
	WebPaParameter InterfaceDeviceWifiEnabledParameter = new WebPaParameter();
	List<WebPaParameter> webPaParameters = new ArrayList<>();
	BroadBandResultObject resultObject = null;
	String valueOfWebPaParams = "";
	// Variable declaration Ends
	try {
	    /** Assigning Value of WebpaParams to be set based on input */
	    valueOfWebPaParams = isEnableReport ? BroadBandTestConstants.TRUE : BroadBandTestConstants.FALSE;

	    /**
	     * Setting WebPa Params for InterfaceDeviceWiFi Statistics Enabled
	     */
	    InterfaceDeviceWifiEnabledParameter.setName(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
	    InterfaceDeviceWifiEnabledParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
	    InterfaceDeviceWifiEnabledParameter.setValue(valueOfWebPaParams);
	    webPaParameters.add(InterfaceDeviceWifiEnabledParameter);

	    /** Setting WebPa Params for RadioInterfaceStatistics Enabled */
	    radioInterfaceStatisticsEnabledParameter.setName(BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
	    radioInterfaceStatisticsEnabledParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
	    radioInterfaceStatisticsEnabledParameter.setValue(valueOfWebPaParams);
	    webPaParameters.add(radioInterfaceStatisticsEnabledParameter);

	    /**
	     * Executing Multiple Webpa Params for InterfaceDeviceWiFi & RadioInterfaceStatistics Enabled or Disabled
	     */
	    resultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
		    webPaParameters);
	    enablerorDisableStatus = resultObject.isStatus();
	    errorMessage = resultObject.getErrorMessage();
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	LOGGER.debug("ENDING METHOD enableOrDisableHarvesterReport");
	return enablerorDisableStatus;
    }

    /**
     * Method to set Reporting & Polling Period for InterfaceDevicesWifi & RadioInterfaceStatistics to default values
     * 
     * @param device
     *            {@link Dut}
     * @return true if all default values are set successfully
     * @refactor Rakesh C N
     */
    public boolean setReportingAndPollingPeriodToDefault(Dut device) {
	LOGGER.debug("ENTERING METHOD setReportingAndPollingPeriodToDefault");
	// Variable declaration statrts
	boolean areValuesSetToDefault = false;
	String interfaceReportingPeriodDefaultValue = null;
	String radioReportingDefaultValue = null;
	String interfacePollingPeriodDefaultValue = null;
	String radioPollingDefaultValue = null;
	WebPaParameter interfaceDevicesreportingPeriodParameter = new WebPaParameter();
	WebPaParameter interfaceStatisticsReportingPeriodParameter = new WebPaParameter();
	WebPaParameter interfaceDevicesPollingPeriodParameter = new WebPaParameter();
	WebPaParameter interfaceStatisticsPollingPeriodParameter = new WebPaParameter();
	List<WebPaParameter> webPaParameters = new ArrayList<>();
	BroadBandResultObject resultObject = null;
	String errorMessage = "";
	// Variable declaration Ends
	try {
	    /**
	     * Getting default value for InterfaceDevicesWiFi Reporting Period
	     */
	    interfaceReportingPeriodDefaultValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device,
		    tapEnv, BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_DEFAULT_REPORTING_PERIOD);

	    /**
	     * Getting default value for RadioInterfaceStatistics Reporting Period
	     */
	    radioReportingDefaultValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_DEFAULT_REPORTING_PERIOD);

	    /** Getting default value for InterfaceDevicesWiFi Polling Period */
	    interfacePollingPeriodDefaultValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_DEFAULT_POLLING_PERIOD);

	    /**
	     * Getting default value for RadioInterfaceStatistics Polling Period
	     */
	    radioPollingDefaultValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_DEFAULT_POLLING_PERIOD);

	    /** Setting WebPa Params for InterfaceDevicesWifi ReportingPeriod */
	    interfaceDevicesreportingPeriodParameter
		    .setName(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_REPORTING_PERIOD);
	    interfaceDevicesreportingPeriodParameter.setDataType(BroadBandTestConstants.CONSTANT_2);
	    interfaceDevicesreportingPeriodParameter.setValue(interfaceReportingPeriodDefaultValue);
	    webPaParameters.add(interfaceDevicesreportingPeriodParameter);

	    /**
	     * Setting WebPa Params for RadioInterfaceStatistics ReportingPeriod
	     */
	    interfaceStatisticsReportingPeriodParameter
		    .setName(BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_REPORTING_PERIOD);
	    interfaceStatisticsReportingPeriodParameter.setDataType(BroadBandTestConstants.CONSTANT_2);
	    interfaceStatisticsReportingPeriodParameter.setValue(radioReportingDefaultValue);
	    webPaParameters.add(interfaceStatisticsReportingPeriodParameter);

	    /** Setting WebPa Params for InterfaceDevicesWifi PollingPeriod */
	    interfaceDevicesPollingPeriodParameter
		    .setName(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_POLLING_PERIOD);
	    interfaceDevicesPollingPeriodParameter.setDataType(BroadBandTestConstants.CONSTANT_2);
	    interfaceDevicesPollingPeriodParameter.setValue(interfacePollingPeriodDefaultValue);
	    webPaParameters.add(interfaceDevicesPollingPeriodParameter);

	    /**
	     * Setting WebPa Params for RadioInterfaceStatistics PollingPeriod
	     */
	    interfaceStatisticsPollingPeriodParameter
		    .setName(BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS_POLLING_PERIOD);
	    interfaceStatisticsPollingPeriodParameter.setDataType(BroadBandTestConstants.CONSTANT_2);
	    interfaceStatisticsPollingPeriodParameter.setValue(radioPollingDefaultValue);
	    webPaParameters.add(interfaceStatisticsPollingPeriodParameter);

	    /**
	     * Executing Multiple Webpa Params for InterfaceDeviceWiFi & RadioInterfaceStatistics
	     */
	    resultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
		    webPaParameters);
	    areValuesSetToDefault = resultObject.isStatus();
	    errorMessage = resultObject.getErrorMessage();
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	LOGGER.debug("ENDING METHOD setReportingAndPollingPeriodToDefault");
	return areValuesSetToDefault;
    }

}
