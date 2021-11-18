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
package com.automatics.rdkb.networkconnectivitytest;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaServerResponse;
import com.automatics.rdkb.utils.networkconnectivity.BroadBandNetworkConnectivityUtils;
import com.automatics.rdkb.utils.CommonUtils;

/**
 * 
 * Test class for testing the network Connectivity and checking the Ping default
 * values.
 * 
 * @author Joseph_Maduram
 * @Refactor Govardhan
 *
 */
public class BroadBandNetworkConnectivityTest extends AutomaticsTestBase {
	/**
	 * 
	 * 
	 * TC-RDKB-NW-CONNECTIVITY-1001: Verify the ping related default values in the
	 * device after factory resetting by Webpa.
	 *
	 * <ol>
	 * <li>S1)Verification of Number of pings per server using the TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer
	 * to get the default NumPingperServer</li>
	 * <li>S2)Verification of Minimum Number of ping server using the TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer
	 * to get the default MinNumPingServer</li>
	 * <li>S3)Verification of ping Interval using the TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	 * get the default pingInterval</li>
	 * <li>S4)Verification of ping response wait time using the TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime
	 * to get the default PingRespWaitTime</li>
	 * <li>S5)Verification of usage compute window using the TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow
	 * to get the default UsageComputeWindow value</li>
	 * <li>S6)Verification of AvgCPUThreshold using the TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgCPUThreshold
	 * to get default the AvgCPUThreshold</li>
	 * <li>S7)Verification of AvgMemoryThreshold using the TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgMemoryThreshold
	 * to get the default AvgMemoryThreshold</li>
	 * <li>S8)Verification of MaxRebootCount using the TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxRebootCount to
	 * get the default MaxRebootCount</li>
	 * <li>S9)Verification of MaxResetCount using the TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxResetCount to
	 * get the default MaxResetCount value</li>
	 * <li>S10)Verification of TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	 * set the pingInterval as 14</li>
	 * <li>S11)Verification of TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	 * set the Invalid pingInterval as 1441</li>
	 * <li>S12) Verification of TR181
	 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
	 * set the valid pingInterval between 14 and 1440</li>
	 * </ol>
	 *
	 * @author Joseph_Maduram
	 * @Refactor Govardhan
	 * 
	 * @param device {@link Dut}
	 */

	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
			BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1001")
	public void testNetworkConnectivityForDefaultValuesUsingWebpa(Dut device) {
		// String to store the test case ID
		String testId = "TC-RDKB-NW-CONNECTIVITY-101";
		// String to store the test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// stores the test status
		boolean status = false;
		// Stores the boolean value
		boolean isFactoryReset = false;
		// stores the response
		String response = null;
		// stores the webPaServer Response
		WebPaServerResponse webPaServerResponse = null;

		// stores default maxResetCount value
		String defaultMaxResetCount = BroadBandTestConstants.STRING_VALUE_THREE;
		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION:DESCRIPTION : Factory reset the device to check default values");
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION : ACTION : performing factory reset by webpa");
			LOGGER.info("PRE-CONDITION : EXPECTED : The device should get factory resetted by webpa");
			if (!BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device)) {
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "Factory Resetting the device failed");
			}
			isFactoryReset = true;
			LOGGER.info("PRE-CONDITION : ACTUAL: Factory Reset is successful");
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1001");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the ping related default values in the device after factory resetting by  WebPA.");

			/**
			 * STEP 1:Verification of Number of pings per server using the TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer
			 * to get the default NumPingperServer
			 * 
			 */
			tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			testStepNumber = "s1";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 1:Verification of Number of pings per server using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer to get the default NumPingperServer");
			LOGGER.info("EXPECTED-NumPingperServer should have the default value as 3");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.NUM_PINGS_PER_SERVER);
			LOGGER.info("NumPingPerServer retrieved using WebPa = " + response);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
			errorMessage = "Unable to verify  the NumPingsPerServer using WebPA command on TR181 parameter'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer' -Expected value:"
					+ BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response;
			LOGGER.info("S1 ACTUAL : " + (status
					? "Successfully verified the default NumPingperServer 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_NumPingsPerServer' using WebPA command.-Expected value:"
							+ BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 2:Verification of Minimum Number of ping server using the TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer
			 * to get the default MinNumPingServer
			 */

			testStepNumber = "s2";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 2:Verification of Minimum Number of ping server using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer to get the default MinNumPingServer");
			LOGGER.info("EXPECTED-MinNumPingServer should have the default value as 1");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MIN_NUM_PINGS_SERVER);
			LOGGER.info("minNumPingServer retrieved using WebPa = " + response);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
			errorMessage = "Unable to verify the MinNumPingServer using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer' -Expected value:"
					+ BroadBandTestConstants.STRING_VALUE_ONE + "|Actual Value:" + response;
			LOGGER.info("S2 ACTUAL : " + (status
					? "Successfully verified the default MinNumPingServer 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MinNumPingServer' using WebPA command.-Expected value:"
							+ BroadBandTestConstants.STRING_VALUE_ONE + "|Actual Value:" + response
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 3:Verification of ping Interval using the TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
			 * get the default pingInterval
			 */

			testStepNumber = "s3";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 3:Verification of ping Interval using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to get the default pingInterval");
			LOGGER.info("EXPECTED-pingInterval should have the default value as 60");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.PING_INTERVAL);
			LOGGER.info("pingInterval retrieved using WebPa = " + response);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL);
			errorMessage = "Unable to verify the pingInterval  using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval ' -Expected value:"
					+ BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL + "|Actual Value:" + response;
			LOGGER.info("S3 ACTUAL : " + (status
					? "Successfully verified the default pingInterval  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command. -Expected value:"
							+ BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL + "|Actual Value:" + response
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 4:Verification of ping response wait time using the TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime
			 * to get the default PingRespWaitTime
			 * 
			 */

			testStepNumber = "s4";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 4:Verification of ping response wait time using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime to get the default PingRespWaitTime");
			LOGGER.info("EXPECTED-PingRespWaitTime should have the default value as 1000");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.PING_RESP_WAIT_TIME);
			LOGGER.info("pingRespWaitTime retrieved using WebPa = " + response);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_PING_RESP_WAIT_TIME);
			errorMessage = "Unable to verify the pingRespWaitTime using WebPA command on TR181 parameter  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime' -Expected value:"
					+ BroadBandTestConstants.CONSTANT_PING_RESP_WAIT_TIME + "|Actual Value:" + response;
			LOGGER.info("S4 ACTUAL : " + (status
					? "Successfully verified the default PingRespWaitTime 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_PingRespWaitTime' using WebPA command.-Expected value:"
							+ BroadBandTestConstants.CONSTANT_PING_RESP_WAIT_TIME + "|Actual Value:" + response
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 5:Verification of usage compute window using the TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow
			 * to get the default UsageComputeWindow value
			 */

			testStepNumber = "s5";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 5:Verification of usage compute window using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow  to get the default UsageComputeWindow value");
			LOGGER.info("EXPECTED-UsageComputeWindow should have the default value as 15");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW);
			LOGGER.info("UsageComputeWindow retrieved using WebPa = " + response);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW);
			errorMessage = "Unable to verify the UsageComputeWindow  using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow'  -Expected value:"
					+ BroadBandTestConstants.CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW + "|Actual Value:" + response;
			LOGGER.info("S5 ACTUAL : " + (status
					? "Successfully verified the default UsageComputeWindow  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow' using WebPA command.-Expected value:"
							+ BroadBandTestConstants.CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW + "|Actual Value:"
							+ response
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 6:Verification of AvgCPUThreshold using the TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgCPUThreshold
			 * to get default the AvgCPUThreshold
			 */

			testStepNumber = "s6";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 6:Verification of AvgCPUThreshold using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgCPUThreshold  to get the default AvgCPUThreshold ");
			LOGGER.info("EXPECTED-AvgCPUThreshold should have the default value as 100");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.AVG_CPU_THRESHOLD);
			LOGGER.info("AvgCPUThreshold retrieved using WebPa = " + response);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_AVG_CPU_THRESHOLD);
			errorMessage = "Unable to verify the avgCPUThreshold using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgCPUThreshold'-Expected value:"
					+ BroadBandTestConstants.CONSTANT_AVG_CPU_THRESHOLD + "|Actual Value:" + response;
			LOGGER.info("S6 ACTUAL : " + (status
					? "Successfully verified the default AvgCPUThreshold  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgCPUThreshold' using WebPA command.-Expected value:"
							+ BroadBandTestConstants.CONSTANT_AVG_CPU_THRESHOLD + "|Actual Value:" + response
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 7:Verification of AvgMemoryThreshold using the TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgMemoryThreshold
			 * to get the default AvgMemoryThreshold
			 */

			testStepNumber = "s7";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 7:Verification of AvgMemoryThreshold using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgMemoryThreshold  to get the default AvgMemoryThreshold");
			LOGGER.info("EXPECTED-AvgMemoryThreshold should have the default value as 100");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.AVG_MEMORY_THRESHOLD);
			LOGGER.info("AvgMemoryThreshold retrieved using WebPa = " + response);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_AVG_MEMORY_THRESHOLD);
			errorMessage = "Unable to verify the AvgMemoryThreshold using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgMemoryThreshold' -Expected value:"
					+ BroadBandTestConstants.CONSTANT_AVG_MEMORY_THRESHOLD + "|Actual Value:" + response;
			LOGGER.info("S7 ACTUAL : " + (status
					? "Successfully verified the default AvgMemoryThreshold  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_AvgMemoryThreshold' using WebPA command. -Expected value:"
							+ BroadBandTestConstants.CONSTANT_AVG_MEMORY_THRESHOLD + "|Actual Value:" + response
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 8: Verification of MaxRebootCount using the TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxRebootCount to
			 * get the default MaxRebootCount value
			 */

			testStepNumber = "s8";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 8:Verification of MaxRebootCount using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxRebootCount  to get the default MaxRebootCount");
			LOGGER.info("EXPECTED-MaxRebootCount should have the default value as 3");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MAX_REBOOT_COUNT);
			LOGGER.info("MaxRebootCount retrieved using WebPa = " + response);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
			errorMessage = "Unable to verify the MaxRebootCount using WebPA command on TR181 parameter'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxRebootCount' -Expected value:"
					+ BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response;
			LOGGER.info("S8 ACTUAL : " + (status
					? "Successfully verified the default MaxRebootCount  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxRebootCount' using WebPA command.-Expected value:"
							+ BroadBandTestConstants.STRING_VALUE_THREE + "|Actual Value:" + response
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 9:Verification of MaxResetCount using the TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxResetCount to
			 * get the default MaxResetCount value
			 */

			testStepNumber = "s9";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 9:Verification of MaxResetCount using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxResetCount  to get the default MaxResetCount value");
			LOGGER.info(
					"EXPECTED-MaxResetCount should have the default value as 3 for master ,stable and release build");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.MAX_RESET_COUNT);
			LOGGER.info("MaxResetCount retrieved using WebPa = " + response);
			defaultMaxResetCount = AutomaticsPropertyUtility
					.getProperty(BroadBandTestConstants.DEFAULT_MAX_RESET_COUNT);
			status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(defaultMaxResetCount);
			errorMessage = "Unable to verify the MaxResetCount using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxResetCount' -Expected value:"
					+ defaultMaxResetCount + "|Actual Value:" + response;
			LOGGER.info("S9 ACTUAL : " + (status
					? "Successfully verified the default MaxResetCount  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_MaxResetCount' using WebPA command.-Expected value:"
							+ defaultMaxResetCount + "|Actual Value:" + response
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 10:Verification of TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
			 * set the Invalid pingInterval as 14
			 */

			testStepNumber = "s10";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 10:Verification of  TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to set the pingInterval as 14");
			LOGGER.info("EXPECTED-Should not be able to set the pinginterval as 14");
			LOGGER.info("#####################################################################################");
			errorMessage = "Able to set  the Invalid ping interval for 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command.";
			webPaServerResponse = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
					BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_PING_INTERVAL_14,
					BroadBandTestConstants.CONSTANT_2);
			tapEnv.waitTill(BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.INVALID_PARAMETER_VALUE);
			LOGGER.info(
					"S10 webPaServerResponse is : " + webPaServerResponse.getMessage() + " & Status is : " + status);
			LOGGER.info("S10 ACTUAL : " + (status
					? " Not able to set the Invalid Ping Interval as 14 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command."
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 11:Verification of TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
			 * set the Invalid pingInterval as 1441
			 */

			testStepNumber = "s11";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 11:Verification of TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to set the pingInterval as 1441");
			LOGGER.info("EXPECTED-Should not be able to set the pinginterval as 1441");
			LOGGER.info("#####################################################################################");
			errorMessage = "Able to set  the Invalid ping interval for 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command.";

			webPaServerResponse = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
					BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_PING_INTERVAL_1441,
					BroadBandTestConstants.CONSTANT_2);
			tapEnv.waitTill(BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.INVALID_PARAMETER_VALUE);
			LOGGER.info(
					"S11 webPaServerResponse is : " + webPaServerResponse.getMessage() + " & Status is : " + status);
			LOGGER.info("S11 ACTUAL : " + (status
					? " Not able to set the Invalid Ping Interval as 1441 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command."
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 12:Verification of TR181
			 * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval to
			 * set the valid pingInterval between 14 and 1440
			 */

			testStepNumber = "s12";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 12:Verification of  TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval  to set the valid pingInterval between 14 and 1440");
			LOGGER.info("EXPECTED-Should  be able to set the pinginterval between 14 and 1440");
			LOGGER.info("#####################################################################################");
			status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.PING_INTERVAL,
					BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL, BroadBandTestConstants.CONSTANT_2);
			errorMessage = "Unable to set  the valid ping interval between 14 and 1440 for 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command.";
			LOGGER.info("S12 ACTUAL : " + (status
					? "able to set the valid Ping Interval between 14 and 1440 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval' using WebPA command."
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		}

		catch (Exception testException) {
			errorMessage = "Exception occured while checking the default Values: " + testException.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			if (isFactoryReset) {

				LOGGER.info("POST-CONDITION : DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION.");
				LOGGER.info("POST-CONDITION : ACTION : BROAD BAND DEVICE REACTIVATION. ");
				LOGGER.info("POST-CONDITION : EXPECTED : device should get reactivated");
				LOGGER.info("### POST-CONDITION ### BEGIN BROAD BAND DEVICE REACTIVATION.");
				BroadBandWiFiUtils.reactivateBroadBandDeviceWebPa(tapEnv, device);
				LOGGER.info("### POST-CONDITION ### END BROAD BAND DEVICE REACTIVATION.");
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			}
		}

	}

	/**
	 * Test case is created as part of COVERAGE AUTOMATION based on the RDKB-5275
	 * Ping Server to detect network connectivity and RDKB-6020 Connectivity test
	 * using Gateway IP
	 * 
	 * 
	 * TC-RDKB-NW-CONNECTIVITY-1003: Verify whether valid ping servers are set by
	 * Webpa and the ping success logs are captured in selfHeallog.txt.0
	 *
	 * <ol>
	 * <li>PRE CONDITION 1:verify Webpa process is up and running</li>
	 * <li>PRE CONDITION 2:Check and Set the Ping Interval to 15 mins</li>
	 * 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'using
	 * WebPA.</li>
	 * <li>S1)Verification of adding valid Ipv4 ping server as in table row 1 by
	 * using Webpa POST command on
	 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."</li>
	 * <li>S2)Verification of adding valid Ipv4 ping server as in table row 2 by
	 * using Webpa POST command on
	 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."</li>
	 * <li>S3)Verification of adding valid Ipv4 ping server as in table row 3 by
	 * using Webpa POST command on
	 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."</li>
	 * <li>S4)Verification of adding valid Ipv6 ping server as in table row 1 by
	 * using Webpa POST command on
	 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."</li>
	 * <li>S5)Verification of adding valid Ipv6 ping server as in table row 2 by
	 * using Webpa POST command on
	 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."</li>
	 * <li>S6)Verification of adding valid Ipv6 ping server as in table row 3 by
	 * using Webpa POST command on
	 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."</li>
	 * <li>S7)verify whether "Connectivity Test is Successfull" message is present
	 * in self heallog.txt.0 after setting the Valid servers.</li>
	 * <li>S8)verify whether "GW IP Connectivity Test Successfull" message is
	 * present in rdklogs/logs/selfheallog.txt.0 .</li>
	 * <li>S9)verify whether 'Connectivity Test is Successfull' message is present
	 * in self heallog.txt.0 after the setting the Valid ping servers in the next
	 * cycle for set ping interval time of 15 mins.</li>
	 * <li>POST-CONDITION 1: Delete the added ping servers in the Ping Server Table
	 * by using WEBPA DELETE command.</li>
	 * <li>POST-CONDITION 2: Set the ping interval to default value.</li>
	 * </ol>
	 *
	 * @author Joseph Maduram
	 * @Refactor Athira
	 * 
	 * @param device {@link Dut}
	 */

	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
			BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-NW-CONNECTIVITY-1003")

	public void validPingServerConnectivityTest(Dut device) {
		// Variable Declaration begins
		// String to store the test case ID
		String testId = "TC-RDKB-NW-CONNECTIVITY-103";
		// String to store the test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// String to store the added table row number
		String tableRowNumber = null;
		// List of String to store the added table row numbers
		List<String> tableRow = new ArrayList<String>();
		// stores the test status
		boolean status = false;
		// string to store the WebPA server response
		WebPaServerResponse webPaServerResponse = null;
		Map<String, List<String>> pingServersTable = new HashMap<String, List<String>>();
		// List of String to store the ping servers
		List<String> pingServers = new ArrayList<String>();
		String ipv4PingServer = null;
		String ipv6PingServer = null;
		String defaultPingInterval = null;
		String response = null;
		// Variable Declaration ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-NW-CONNECTIVITY-1003");
		LOGGER.info(
				"TEST DESCRIPTION:Verify whether valid IP address are set for the  ping servers by Webpa and the ping success logs are captured in selfHeallog.txt.0");
		LOGGER.info(
				"NOTES : This test case is written as part of validating RDKB-5275 and RDKB-6020  [Coverage Automation]");
		LOGGER.info("PRE-CONDITION 1: verify Webpa process is up and running");
		LOGGER.info("PRE-CONDITION 2: Check and Set the Ping Interval to 15 mins");
		LOGGER.info(
				"STEP 1 :Verification of adding valid Ipv4 ping server as in table row 1 by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
		LOGGER.info(
				"STEP 2 :Verification of adding valid Ipv4 ping server as in table row 2 by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
		LOGGER.info(
				"STEP 3:Verification of adding valid Ipv4 ping server as in table row 3 by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
		LOGGER.info(
				"STEP 4:Verification of adding valid Ipv6 ping server as in table row 1by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
		LOGGER.info(
				"STEP 5: Verification of adding valid Ipv6 ping server as in table row 2 by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
		LOGGER.info(
				"STEP 6:Verification of adding valid Ipv6 ping server as in table row 3 by using Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.");
		LOGGER.info(
				"STEP 7:verify whether Connectivity Test is Successfull message is present in self heallog.txt.0 after setting the Valid servers.");
		LOGGER.info(
				"STEP 8:verify whether GW IP Connectivity Test Successfull message is present in rdklogs/logs/selfheallog.txt.0");
		LOGGER.info(
				"STEP 9:verify whether 'Connectivity Test is Successfull' message  is present in self heallog.txt.0  after the setting the Valid ping servers in the next cycle for set ping interval time of 15 mins.");
		LOGGER.info(
				"POST CONDITION 1:Delete the added ping servers in the Ping Server Table by using WEBPA DELETE command");
		LOGGER.info("POST CONDITION 2:Set the ping interval to default value.");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION 1:DESCRIPTION : verify Webpa process is up and running");
			LOGGER.info(
					"PRE-CONDITION 1: ACTION : Execute Device.DeviceInfo.SerialNumber and verify Webpa process is up and running'");
			LOGGER.info("PRE-CONDITION 1: EXPECTED : Webpa process should be up and running");
			LOGGER.info("#######################################################################################");

			// If last (third) parameter in verifyWebPaProcessIsUp is set to true, this
			// command could
			// potentially run for 8 minutes. As false, it will run no longer than 30
			// seconds.
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, false);

			if (status) {
				LOGGER.info("PRE-CONDITION 1: ACTUAL :WEBPA IS UP AND RUNNING - CONTINUING THE EXECUTION");
			} else {
				LOGGER.error("PRE-CONDITION 1: ACTUAL : Pre condition failed");
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
						+ "WEBPA IS NOT UP AND RUNNING - HENCE BLOCKING THE EXECUTION");
			}
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION 2:DESCRIPTION : Check and Set the Ping Interval to 15 mins using WebPA.");
			LOGGER.info(
					"PRE-CONDITION 2: ACTION : Set the ping Interval to 15 mins using WebPA-'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_pingInterval'");
			LOGGER.info("PRE-CONDITION 2: EXPECTED : ping Interval should be set to 15 mins");
			LOGGER.info("#######################################################################################");
			defaultPingInterval = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.PING_INTERVAL);
			if (defaultPingInterval.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_PING_INTERVAL)) {
				status = true;
			} else {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.PING_INTERVAL, BroadBandTestConstants.CONSTANT_2,
						BroadBandTestConstants.CONSTANT_PING_INTERVAL);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION 2: ACTUAL : Successfully Set the ping interval to 15 mins");
			} else {
				LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
						+ "UNABLE TO SET THE PING INTERVAL TO 15 MINS - HENCE BLOCKING THE EXECUTION");
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1:Verification of adding valid Ipv4 ping server in table row 1 by using
			 * Webpa POST command on
			 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."
			 * 
			 */
			testStepNumber = "s1";
			errorMessage = "Unable to set the  First IPV4 ping server URI with valid Ipv4 ping server using WebPA command.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1:DESCRIPTION: Verification of adding valid Ipv4 ping server in table row 1 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
			LOGGER.info(
					"STEP 1:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable. to set the table row 1 with valid ipv4 address");
			LOGGER.info("STEP 1:EXPECTED: should be able to set the IPV4 ping server1  by WebPA");
			LOGGER.info("**********************************************************************************");
			ipv4PingServer = BroadBandNetworkConnectivityUtils.retrievePingServerUsingNslookUpForIpv4(device, tapEnv);
			pingServers.add(ipv4PingServer);
			pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
			tableRowNumber = webPaServerResponse.getRow();
			tableRow.add(tableRowNumber);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : Successfully added the valid Ipv4 ping server as  First IPV4 ping server URI using WebPA command");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2:Verification of adding valid Ipv4 ping server in table row 2 by using
			 * Webpa POST command on
			 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."
			 * 
			 */

			testStepNumber = "s2";
			errorMessage = "Unable to set the  Second IPV4 ping server URI with valid Ipv4 ping server using WebPA command.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2:DESCRIPTION: Verification of adding valid Ipv4 ping server in table row 2 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
			LOGGER.info(
					"STEP 2:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable. to set the table row 2 with valid ipv4 address.");
			LOGGER.info("STEP 2:EXPECTED: should be able to set the IPV4 ping server2 by WebPA");
			LOGGER.info("**********************************************************************************");
			pingServersTable.clear();
			pingServers.clear();
			pingServers.add(ipv4PingServer);
			pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
			tableRowNumber = webPaServerResponse.getRow();
			tableRow.add(tableRowNumber);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : Successfully added the valid Ipv4 ping server as Second IPV4 ping server URI using WebPA command.");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3:Verification of adding valid Ipv4 ping server in table row 3 by using
			 * Webpa POST command on
			 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."
			 * 
			 */

			testStepNumber = "s3";
			errorMessage = "Unable to set the  Third IPV4 ping server URI with valid Ipv4 ping server using WebPA command.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3:DESCRIPTION:Verification of adding valid Ipv4 ping server in table row 3 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.'");
			LOGGER.info(
					"STEP 3:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable. to set the table row 3 with valid ipv4 address");
			LOGGER.info("STEP 3:EXPECTED: should be able to set the IPV4 ping server3  by WebPA");
			LOGGER.info("**********************************************************************************");
			pingServersTable.clear();
			pingServers.clear();
			pingServers.add(ipv4PingServer);
			pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
			tableRowNumber = webPaServerResponse.getRow();
			tableRow.add(tableRowNumber);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Successfully added the valid Ipv4 ping server as  Third IPV4 ping server URI using WebPA command.");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			/**
			 * STEP 4:Verification of adding valid Ipv6 ping server in table row 1 by using
			 * Webpa POST command on
			 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."
			 * 
			 * 
			 */
			testStepNumber = "s4";
			errorMessage = "Unable to set the  First IPV6 ping server URI with valid Ipv6 ping server using WebPA command.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4:DESCRIPTION:Verification of adding valid Ipv6 ping server  in table row 1 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
			LOGGER.info(
					"STEP 4:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable. to set the table row 1 with valid ipv6 address");
			LOGGER.info("STEP 4:EXPECTED: should be able to set the IPV6 ping server1 by WebPA");
			LOGGER.info("**********************************************************************************");
			pingServersTable.clear();
			pingServers.clear();
			ipv6PingServer = BroadBandNetworkConnectivityUtils.retrievePingServerUsingNslookUpForIpv6(device, tapEnv);
			LOGGER.info("Ipv6 Ping Server Address = " + ipv6PingServer);
			pingServers.add(ipv6PingServer);
			pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
			tableRowNumber = webPaServerResponse.getRow();
			tableRow.add(tableRowNumber);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Successfully added the valid Ipv6 ping server as  First IPV6 ping server URI using WebPA command.");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 5:Verification of adding valid Ipv6 ping server in table row 2 by using
			 * Webpa POST command on
			 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."
			 * 
			 */
			testStepNumber = "s5";
			errorMessage = "Unable to set the  Second IPV6 ping server URI with valid Ipv6 ping server using WebPA command.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5:DESCRIPTION:Verification of adding valid Ipv6 ping server in table row 2 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
			LOGGER.info(
					"STEP 5:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable. to set the table row 2 with valid ipv6 address");
			LOGGER.info("STEP 5:EXPECTED: should be able to set the IPV6 ping server2 by WebPA");
			LOGGER.info("**********************************************************************************");
			pingServersTable.clear();
			pingServers.clear();
			pingServers.add(ipv6PingServer);
			pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
			tableRowNumber = webPaServerResponse.getRow();
			tableRow.add(tableRowNumber);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : Successfully added the valid Ipv6 ping server as second IPV6 ping server URI using WebPA command");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 6:Verification of adding valid Ipv6 ping server in table row 3 by using
			 * Webpa POST command on
			 * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable."
			 * 
			 */

			testStepNumber = "s6";
			errorMessage = "Unable to set the  Third IPV6 ping server URI with valid Ipv6 ping server using WebPA command.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6:DESCRIPTION:Verification of adding valid Ipv6 ping server  in table row 3 by using  Webpa POST command on 'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.'");
			LOGGER.info(
					"STEP 6:ACTION: Execute Webpa POST command on Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable. to set the table row 3 with valid ipv6 address");
			LOGGER.info("STEP 6:EXPECTED: should be able to set the IPV6 ping server3 by WebPA");
			LOGGER.info("**********************************************************************************");
			pingServersTable.clear();
			pingServers.clear();
			pingServers.add(ipv6PingServer);
			pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
			tableRowNumber = webPaServerResponse.getRow();
			tableRow.add(tableRowNumber);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL : Successfully added the valid Ipv6 ping server as third IPV6 ping server URI using WebPA command");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 7 :verify whether "Connectivity Test is Successfull" message is present
			 * in self heallog.txt.0 after the setting the Valid servers.
			 */
			testStepNumber = "s7";
			status = false;
			errorMessage = "Ping server connectivity success message is not logged in the next self heal window";
			String timeFromFirstLog = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 7:DESCRIPTION:verify whether 'Connectivity Test is Successfull' message  is present in self heallog.txt.0  after the setting the Valid ping servers. ");
			LOGGER.info(
					"STEP 7:ACTION: validate whether Connectivity Test is Successfull message  is present in self heallog.txt.0 after setting the required servers");
			LOGGER.info(
					"STEP 7:EXPECTED - Valid Ping server connectivity Test success message  should  get logged in the next self heal window");
			LOGGER.info("**********************************************************************************");
			try {
				response = BroadBandCommonUtils.searchLogByPolling(tapEnv, device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
								BroadBandTraceConstants.PING_SERVER_SUCCESS_LOG_MESSAGE,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.SYMBOL_PIPE,
								BroadBandTestConstants.CMD_TAIL_1),
						BroadBandTestConstants.SIXTY_MINUTES_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
						BroadBandTraceConstants.PING_SERVER_SUCCESS_LOG_MESSAGE.replace(
								BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING));
				if (status) {
					timeFromFirstLog = CommonMethods.patternFinder(response,
							BroadBandTestConstants.PATTERN_FINDER_TO_GREP_TIMESTAMP);
					LOGGER.info("Timestamp from the log is-" + timeFromFirstLog);
				}
			} catch (Exception e) {
				LOGGER.error(
						"Exception occurred while verifying connectivity message in selfheallog: " + e.getMessage());
			}
			if (status) {

				LOGGER.info(
						"STEP 7: ACTUAL : Ping server connectivity success message is logged in the self heal window");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			
			/**
			 * STEP 8 :verify whether "GW IP Connectivity Test Successfull" message is
			 * present in rdklogs/logs/selfheallog.txt.0 .
			 */
			testStepNumber = "s8";
			errorMessage = "Gateway connectivity success message is not logged in the self heal window";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 8:DESCRIPTION: verify whether 'GW IP Connectivity Test Successfull' message  is present in rdklogs/logs/selfheallog.txt.0 . ");
			LOGGER.info(
					"STEP 8:ACTION:validate whether GW IP Connectivity Test Successfull message  is present in rdklogs/logs/selfheallog.txt.0");
			LOGGER.info(
					"STEP 8:EXPECTED - Gateway connectivity success message  should  get logged in the  self heal window");
			LOGGER.info("**********************************************************************************");
			response = BroadBandCommonUtils.searchLogByPolling(tapEnv, device,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
							BroadBandTraceConstants.PING_SERVER_GATEWAY_IP_SUCCESS_LOG_MESSAGE,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_SELFHEAL_LOG,
							BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.CMD_TAIL_1),
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
					BroadBandTraceConstants.PING_SERVER_GATEWAY_IP_SUCCESS_LOG_MESSAGE
							.replace(BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING));
			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Gateway connectivity is successful in the self heal window");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 9 :verify whether 'Connectivity Test is Successfull' message is present
			 * in self heallog.txt.0 in the next cycle for set ping interval time of 15 mins
			 */
			testStepNumber = "s9";
			errorMessage = "Ping server connectivity success message is not logged in the next cycle self heal window for set ping interval time of 15 mins";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 9:DESCRIPTION: verify whether 'Connectivity Test is Successfull' message  is present in self heallog.txt.0  in the next cycle for set ping interval time of 15 mins.");
			LOGGER.info(
					"STEP 9:ACTION: validate whether Connectivity Test is Successfull message  is present in self heallog.txt.0 in the next cycle for set ping interval time of 15 mins");
			LOGGER.info(
					"STEP 9:EXPECTED - Valid Ping server connectivity Test success message  should  get logged in the next cycle self heal window");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.pingTestForSetIntervalTime(device, tapEnv, timeFromFirstLog);
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL : Ping server connectivity success message is logged in the next cycle self heal window for set ping interval time of 15 mins");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		} catch (Exception testException) {
			errorMessage = "Exception occured while Verifying whether valid IP address are set for the  ping server and the ping success logs are captured in selfHeallog.txt.0 "
					+ testException.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		}
	}
}